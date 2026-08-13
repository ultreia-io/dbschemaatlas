test_that("metadata snapshots round trip list columns and missing descriptions", {
  path <- withr::local_tempdir()
  original <- fixture_metadata()
  expect_invisible(write_metadata(original, path))
  result <- read_metadata(path)
  expect_s3_class(result, "dbschema_metadata")
  expect_equal(result$tables_primary_keys$primary_key_columns, original$tables_primary_keys$primary_key_columns)
  expect_equal(result$tables_foreign_keys$target_columns, original$tables_foreign_keys$target_columns)
  expect_true(is.na(result$tables_description[table == "standalone", description]))
})

test_that("metadata validation reports missing inputs", {
  expect_error(build_schema_model(list()), "Missing metadata component")
  expect_error(read_metadata(withr::local_tempdir()), "Metadata file not found")
  expect_error(write_metadata(fixture_metadata(), ""), "single non-empty")
})
