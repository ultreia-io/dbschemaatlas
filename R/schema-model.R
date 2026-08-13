as_flag <- function(x) {
  result <- rep(FALSE, length(x))
  present <- !is.na(x)
  result[present] <- toupper(as.character(x[present])) %in% c("TRUE", "YES", "1")
  result
}

table_id <- function(schema, table) paste(schema, table, sep = ".")

#' Build a schema model
#'
#' Builds table and column flags, forward dependencies, and reverse usages from
#' a metadata snapshot. Composite keys remain ordered list columns.
#'
#' @param metadata A metadata list or snapshot directory.
#' @return A `dbschema_model` object.
#' @export
build_schema_model <- function(metadata) {
  if (is.character(metadata) && length(metadata) == 1L) metadata <- read_metadata(metadata)
  metadata <- validate_metadata(metadata)
  schemas <- data.table::copy(metadata$schemas_description)
  tables <- data.table::copy(metadata$tables_description)
  columns <- data.table::copy(metadata$tables_columns)
  primary_keys <- data.table::copy(metadata$tables_primary_keys)
  foreign_keys <- data.table::copy(metadata$tables_foreign_keys)

  columns[, mandatory := as_flag(mandatory)]
  columns[, primary_key := FALSE]
  columns[, foreign_key := FALSE]
  if (nrow(primary_keys)) {
    pk <- primary_keys[, list(column = unlist(primary_key_columns)), by = c("schema", "table")]
    columns[pk, primary_key := TRUE, on = c("schema", "table", "column")]
  }
  if (nrow(foreign_keys)) {
    fk <- foreign_keys[, list(column = unlist(columns)), by = c("schema", "table")]
    columns[unique(fk), foreign_key := TRUE, on = c("schema", "table", "column")]
  }

  dependencies <- data.table::copy(foreign_keys)
  if (nrow(dependencies)) {
    dependencies[, table_id := table_id(schema, table)]
    dependencies[, target_table_id := table_id(target_schema, target_table)]
    usages <- dependencies[, list(
      schema = target_schema,
      table = target_table,
      columns = target_columns,
      table_id = target_table_id,
      usage_schema = schema,
      usage_table = table,
      usage_columns = columns,
      usage_table_id = table_id,
      usage_foreign_key = foreign_key
    )]
  } else {
    dependencies[, `:=`(table_id = character(), target_table_id = character())]
    usages <- data.table::data.table(
      schema = character(), table = character(), columns = list(), table_id = character(),
      usage_schema = character(), usage_table = character(), usage_columns = list(),
      usage_table_id = character(), usage_foreign_key = character()
    )
  }

  tables[, table_id := table_id(schema, table)]
  result <- list(
    schemas = schemas,
    tables = tables,
    columns = columns,
    primary_keys = primary_keys,
    foreign_keys = foreign_keys,
    dependencies = dependencies,
    usages = usages
  )
  class(result) <- c("dbschema_model", "list")
  result
}

validate_model <- function(model) {
  if (!inherits(model, "dbschema_model")) {
    stop("`model` must be created by `build_schema_model()`.", call. = FALSE)
  }
  model
}

#' Access schema-model tables
#'
#' @param model A `dbschema_model`.
#' @return A copy of the requested `data.table`.
#' @name model_accessors
NULL

#' @rdname model_accessors
#' @export
model_schemas <- function(model) data.table::copy(validate_model(model)$schemas)
#' @rdname model_accessors
#' @export
model_tables <- function(model) data.table::copy(validate_model(model)$tables)
#' @rdname model_accessors
#' @export
model_columns <- function(model) data.table::copy(validate_model(model)$columns)
#' @rdname model_accessors
#' @export
model_dependencies <- function(model) data.table::copy(validate_model(model)$dependencies)
#' @rdname model_accessors
#' @export
model_usages <- function(model) data.table::copy(validate_model(model)$usages)

normalize_entry_point <- function(entry_point) {
  if (!is.character(entry_point) || length(entry_point) != 1L || is.na(entry_point) || !nzchar(entry_point)) {
    stop("`entry_point` must be one 'schema.table' or 'schema.table.column' value.", call. = FALSE)
  }
  parts <- strsplit(entry_point, ".", fixed = TRUE)[[1L]]
  if (!length(parts) %in% c(2L, 3L) || any(!nzchar(parts))) {
    stop("`entry_point` must be one 'schema.table' or 'schema.table.column' value.", call. = FALSE)
  }
  paste(parts[1:2], collapse = ".")
}

#' Build a dependency tree
#'
#' Traverses both dependencies and reverse usages without revisiting tables.
#'
#' @param model A `dbschema_model`.
#' @param entry_point A `schema.table` or `schema.table.column` identifier.
#' @param max_depth Maximum traversal depth.
#' @return A `data.table` describing the traversal.
#' @export
dependency_tree <- function(model, entry_point, max_depth = Inf) {
  model <- validate_model(model)
  root <- normalize_entry_point(entry_point)
  if (!root %in% model$tables$table_id) stop("Unknown entry point table: ", root, call. = FALSE)
  if (!is.numeric(max_depth) || length(max_depth) != 1L || is.na(max_depth) || max_depth < 0) {
    stop("`max_depth` must be a non-negative number.", call. = FALSE)
  }
  result <- data.table::data.table(
    level = 0L, parent_table = NA_character_, table = root,
    direction = NA_character_, foreign_key = NA_character_, path = root
  )
  visited <- root
  frontier <- root
  level <- 0L
  while (length(frontier) && level < max_depth) {
    level <- level + 1L
    additions <- list()
    for (current in frontier) {
      forward <- model$dependencies[table_id == current]
      if (nrow(forward)) {
        additions[[length(additions) + 1L]] <- data.table::data.table(
          level = level, parent_table = current, table = forward$target_table_id,
          direction = "depends_on", foreign_key = forward$foreign_key,
          path = paste(current, forward$target_table_id, sep = " -> ")
        )
      }
      reverse <- model$dependencies[target_table_id == current]
      if (nrow(reverse)) {
        additions[[length(additions) + 1L]] <- data.table::data.table(
          level = level, parent_table = current, table = reverse$table_id,
          direction = "used_by", foreign_key = reverse$foreign_key,
          path = paste(current, reverse$table_id, sep = " <- ")
        )
      }
    }
    if (!length(additions)) break
    next_rows <- unique(data.table::rbindlist(additions, fill = TRUE), by = "table")
    next_rows <- next_rows[!table %in% visited]
    if (!nrow(next_rows)) break
    result <- data.table::rbindlist(list(result, next_rows), fill = TRUE)
    frontier <- next_rows$table
    visited <- c(visited, frontier)
  }
  result
}
