test_that("extraction orchestrates packaged SQL through dbiutils-compatible delegates", {
  calls <- character()
  raw <- list(
    schemas_description = data.frame(schema_name = "public", description = "Public"),
    tables_description = data.frame(schema_name = "public", table_name = "child", description = "Child"),
    tables_columns = data.frame(schema_name = "public", table_name = "child", column_name = "id", type = "integer", mandatory = "YES", description = "Id"),
    tables_foreign_keys = data.frame(foreign_key = character(), schema_name = character(), table_name = character(), column_name = character(), target_schema = character(), target_table = character(), target_column = character(), column_order = integer()),
    tables_primary_keys = data.frame(schema_name = "public", table_name = "child", column_name = "id", position = 1L)
  )
  index <- 0L
  result <- dbschemaatlas:::extract_schema_metadata_impl(
    "public", function() "connection",
    with_connection = function(provider, code) code(provider()),
    query = function(connection, sql, params) {
      index <<- index + 1L
      calls <<- c(calls, sql)
      raw[[index]]
    },
    sql_reader = function(resource) paste("SQL", resource)
  )
  expect_s3_class(result, "dbschema_metadata")
  expect_length(calls, 5L)
  expect_equal(result$tables_primary_keys$primary_key_columns[[1]], "id")
})

test_that("public extraction explicitly delegates to dbiutils", {
  code <- paste(deparse(body(extract_schema_metadata)), collapse = " ")
  expect_match(code, "dbiutils::with_db_connection", fixed = TRUE)
  expect_match(code, "dbiutils::db_query", fixed = TRUE)
  sql_code <- paste(deparse(body(dbschemaatlas:::read_atlas_sql)), collapse = " ")
  expect_match(sql_code, "dbiutils::read_sql_file", fixed = TRUE)
  expect_error(extract_schema_metadata(character(), identity), "non-empty")
})
