test_that("comment SQL is safely quoted and dry-run does not execute", {
  provider <- function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  statements <- apply_metadata_comments(fixture_metadata(), provider, apply = FALSE)
  expect_true(length(statements) > 0L)
  expect_match(statements[[1]], "COMMENT ON SCHEMA")
  expect_true(any(grepl("COMMENT ON COLUMN", statements, fixed = TRUE)))
  expect_false(any(grepl("standalone", statements, fixed = TRUE)))
})
