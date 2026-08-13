quote_identifier <- function(connection, ...) {
  parts <- list(...)
  paste(vapply(parts, function(part) as.character(DBI::dbQuoteIdentifier(connection, part)), character(1L)), collapse = ".")
}

comment_statements <- function(metadata, connection) {
  statements <- character()
  schemas <- metadata$schemas_description[!is.na(description) & nzchar(description)]
  if (nrow(schemas)) statements <- c(statements, mapply(function(schema, description) {
    sprintf("COMMENT ON SCHEMA %s IS %s;", quote_identifier(connection, schema), DBI::dbQuoteString(connection, description))
  }, schemas$schema, schemas$description, USE.NAMES = FALSE))
  tables <- metadata$tables_description[!is.na(description) & nzchar(description)]
  if (nrow(tables)) statements <- c(statements, mapply(function(schema, table, description) {
    sprintf("COMMENT ON TABLE %s IS %s;", quote_identifier(connection, schema, table), DBI::dbQuoteString(connection, description))
  }, tables$schema, tables$table, tables$description, USE.NAMES = FALSE))
  columns <- metadata$tables_columns[!is.na(description) & nzchar(description)]
  if (nrow(columns)) statements <- c(statements, mapply(function(schema, table, column, description) {
    sprintf("COMMENT ON COLUMN %s IS %s;", quote_identifier(connection, schema, table, column), DBI::dbQuoteString(connection, description))
  }, columns$schema, columns$table, columns$column, columns$description, USE.NAMES = FALSE))
  statements
}

#' Apply stored descriptions to a database
#'
#' @param metadata Metadata list or snapshot directory.
#' @param connection_provider Zero-argument DBI connection provider.
#' @param apply Execute within a transaction when `TRUE`; otherwise return SQL only.
#' @return Character vector of generated statements.
#' @export
apply_metadata_comments <- function(metadata, connection_provider, apply = FALSE) {
  if (is.character(metadata) && length(metadata) == 1L) metadata <- read_metadata(metadata)
  metadata <- validate_metadata(metadata)
  dbiutils::with_db_connection(connection_provider, function(connection) {
    statements <- comment_statements(metadata, connection)
    if (isTRUE(apply) && length(statements)) {
      DBI::dbWithTransaction(connection, lapply(statements, function(statement) DBI::dbExecute(connection, statement)))
    }
    statements
  })
}
