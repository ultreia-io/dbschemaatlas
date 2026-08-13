test_that("installed SQL and template resources resolve", {
  sql <- dbschemaatlas:::atlas_resource("sql", "get_tables_foreign_keys.sql")
  template <- dbschemaatlas:::atlas_resource("templates", "report.Rmd.tpl")
  expect_true(file.exists(sql))
  expect_true(file.exists(template))
  expect_match(dbschemaatlas:::read_atlas_sql("get_tables_foreign_keys.sql"), "pg_constraint")
  expect_error(dbschemaatlas:::atlas_resource("other", "x"), "either")
  expect_error(dbschemaatlas:::atlas_resource("sql", "missing.sql"), "not found")
})
