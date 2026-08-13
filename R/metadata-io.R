metadata_components <- c(
  "schemas_description", "tables_description", "tables_columns",
  "tables_foreign_keys", "tables_primary_keys"
)

empty_foreign_keys <- function() {
  data.table::data.table(
    schema = character(), table = character(), foreign_key = character(),
    columns = list(), target_schema = character(), target_table = character(),
    target_columns = list()
  )
}

empty_primary_keys <- function() {
  data.table::data.table(schema = character(), table = character(), primary_key_columns = list())
}

validate_metadata <- function(metadata) {
  if (!is.list(metadata)) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }
  missing <- setdiff(metadata_components, names(metadata))
  if (length(missing)) {
    stop("Missing metadata component(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  required <- list(
    schemas_description = c("schema", "description"),
    tables_description = c("schema", "table", "description"),
    tables_columns = c("schema", "table", "column", "type", "mandatory", "description"),
    tables_foreign_keys = c("schema", "table", "foreign_key", "columns", "target_schema", "target_table", "target_columns"),
    tables_primary_keys = c("schema", "table", "primary_key_columns")
  )
  result <- lapply(metadata_components, function(name) {
    value <- data.table::as.data.table(metadata[[name]])
    absent <- setdiff(required[[name]], names(value))
    if (length(absent)) {
      stop("Component `", name, "` is missing column(s): ", paste(absent, collapse = ", "), call. = FALSE)
    }
    data.table::copy(value)
  })
  names(result) <- metadata_components
  if (!is.null(metadata$dependencies)) {
    result$dependencies <- data.table::as.data.table(metadata$dependencies)
  }
  class(result) <- c("dbschema_metadata", "list")
  result
}

flatten_list_column <- function(x) {
  vapply(x, paste, collapse = "|", FUN.VALUE = character(1L))
}

#' Save a metadata snapshot
#'
#' @param metadata Metadata returned by [extract_schema_metadata()].
#' @param path Output directory.
#' @return `path`, invisibly.
#' @export
write_metadata <- function(metadata, path) {
  metadata <- validate_metadata(metadata)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty directory path.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (name in metadata_components) {
    value <- data.table::copy(metadata[[name]])
    list_columns <- names(value)[vapply(value, is.list, logical(1L))]
    for (column in list_columns) {
      data.table::set(value, j = column, value = flatten_list_column(value[[column]]))
    }
    data.table::fwrite(value, file.path(path, paste0(name, ".csv")), bom = TRUE, na = "NA")
  }
  if (!is.null(metadata$dependencies)) {
    data.table::fwrite(metadata$dependencies, file.path(path, "dependencies.csv"), bom = TRUE, na = "NA")
  }
  invisible(path)
}

read_component <- function(path, name) {
  file <- file.path(path, paste0(name, ".csv"))
  if (!file.exists(file)) {
    stop("Metadata file not found: ", file, call. = FALSE)
  }
  data.table::fread(file, na.strings = "NA", encoding = "UTF-8")
}

split_list_column <- function(x) {
  lapply(x, function(value) {
    if (is.na(value) || !nzchar(value)) character() else strsplit(value, "|", fixed = TRUE)[[1L]]
  })
}

#' Load a metadata snapshot
#'
#' @param path Directory created by [write_metadata()].
#' @return A `dbschema_metadata` object.
#' @export
read_metadata <- function(path) {
  values <- lapply(metadata_components, function(name) read_component(path, name))
  names(values) <- metadata_components
  values$tables_foreign_keys[, columns := split_list_column(columns)]
  values$tables_foreign_keys[, target_columns := split_list_column(target_columns)]
  values$tables_primary_keys[, primary_key_columns := split_list_column(primary_key_columns)]
  dependency_file <- file.path(path, "dependencies.csv")
  if (file.exists(dependency_file)) {
    values$dependencies <- data.table::fread(dependency_file, na.strings = "NA", encoding = "UTF-8")
  }
  validate_metadata(values)
}
