metadata_queries <- c(
  schemas_description = "get_schemas_description.sql",
  tables_description = "get_tables_description.sql",
  tables_columns = "get_tables_columns.sql",
  tables_foreign_keys = "get_tables_foreign_keys.sql",
  tables_primary_keys = "get_tables_primary_keys.sql"
)

#' Extract PostgreSQL schema metadata
#'
#' Runs the packaged PostgreSQL catalog queries using one short-lived
#' connection supplied by the caller. The connection is managed by
#' [dbiutils::with_db_connection()] and each query is delegated to
#' [dbiutils::db_query()].
#'
#' @param schemas Non-empty character vector of schema names.
#' @param connection_provider Zero-argument function returning a DBI connection.
#' @return A named list of five `data.table` metadata components.
#' @export
extract_schema_metadata <- function(schemas, connection_provider) {
  schemas <- validate_schemas(schemas)
  extract_schema_metadata_impl(
    schemas,
    connection_provider,
    with_connection = dbiutils::with_db_connection,
    query = dbiutils::db_query,
    sql_reader = read_atlas_sql
  )
}

extract_schema_metadata_impl <- function(schemas, connection_provider, with_connection, query, sql_reader) {
  with_connection(connection_provider, function(connection) {
    raw <- lapply(metadata_queries, function(resource) {
      query(connection, sql_reader(resource), params = list(schemas))
    })
    normalize_extracted_metadata(raw)
  })
}

validate_schemas <- function(schemas) {
  if (!is.character(schemas) || !length(schemas) || anyNA(schemas) || any(!nzchar(schemas))) {
    stop("`schemas` must be a non-empty character vector without missing values.", call. = FALSE)
  }
  unique(schemas)
}

rename_columns <- function(x, mapping) {
  x <- data.table::as.data.table(x)
  present <- intersect(names(mapping), names(x))
  if (length(present)) {
    data.table::setnames(x, present, unname(mapping[present]))
  }
  x
}

normalize_extracted_metadata <- function(raw) {
  schemas <- rename_columns(raw$schemas_description, c(schema_name = "schema"))
  tables <- rename_columns(raw$tables_description, c(schema_name = "schema", table_name = "table"))
  columns <- rename_columns(raw$tables_columns, c(schema_name = "schema", table_name = "table", column_name = "column"))
  foreign_keys <- rename_columns(raw$tables_foreign_keys, c(schema_name = "schema", table_name = "table"))
  primary_keys <- rename_columns(raw$tables_primary_keys, c(schema_name = "schema", table_name = "table"))

  if (nrow(foreign_keys)) {
    order_column <- if ("column_order" %in% names(foreign_keys)) "column_order" else "column_name"
    data.table::setorderv(foreign_keys, c("schema", "table", "foreign_key", order_column))
    foreign_keys <- foreign_keys[, list(
      columns = list(.SD[["column_name"]]),
      target_schema = .SD[["target_schema"]][1L],
      target_table = .SD[["target_table"]][1L],
      target_columns = list(.SD[["target_column"]])
    ), by = c("schema", "table", "foreign_key")]
  } else {
    foreign_keys <- empty_foreign_keys()
  }

  if (nrow(primary_keys)) {
    if ("position" %in% names(primary_keys)) {
      data.table::setorderv(primary_keys, c("schema", "table", "position"))
    }
    primary_keys <- primary_keys[, list(primary_key_columns = list(.SD[["column_name"]])), by = c("schema", "table")]
  } else {
    primary_keys <- empty_primary_keys()
  }

  validate_metadata(list(
    schemas_description = schemas[, intersect(c("schema", "description"), names(schemas)), with = FALSE],
    tables_description = tables[, intersect(c("schema", "table", "description"), names(tables)), with = FALSE],
    tables_columns = columns[, intersect(c("schema", "table", "column", "type", "mandatory", "description"), names(columns)), with = FALSE],
    tables_foreign_keys = foreign_keys,
    tables_primary_keys = primary_keys
  ))
}
