test_that("models preserve keys, flags, dependencies, and reverse usages", {
  model <- build_schema_model(fixture_metadata())
  expect_s3_class(model, "dbschema_model")
  expect_equal(nrow(model_schemas(model)), 2L)
  expect_equal(nrow(model_tables(model)), 4L)
  account <- model_columns(model)[schema == "public" & table == "account"]
  expect_equal(account[primary_key == TRUE, column], c("tenant_id", "account_id"))
  expect_false(account[column == "name", foreign_key])
  expect_equal(nrow(model_dependencies(model)), 2L)
  expect_equal(nrow(model_usages(model)[table_id == "public.account"]), 2L)
  expect_equal(model_dependencies(model)[foreign_key == "event_account_fk", target_table_id], "public.account")
})

test_that("dependency trees cover cross-schema links and isolated tables", {
  model <- build_schema_model(fixture_metadata())
  tree <- dependency_tree(model, "public.account.account_id")
  expect_setequal(tree$table, c("public.account", "public.membership", "audit.event"))
  expect_equal(dependency_tree(model, "public.standalone")$table, "public.standalone")
  expect_error(dependency_tree(model, "missing.table"), "Unknown")
  expect_error(dependency_tree(model, "not-valid"), "schema.table")
})

test_that("empty metadata produces a valid empty model", {
  metadata <- list(
    schemas_description = data.table::data.table(schema = character(), description = character()),
    tables_description = data.table::data.table(schema = character(), table = character(), description = character()),
    tables_columns = data.table::data.table(schema = character(), table = character(), column = character(), type = character(), mandatory = character(), description = character()),
    tables_foreign_keys = dbschemaatlas:::empty_foreign_keys(),
    tables_primary_keys = dbschemaatlas:::empty_primary_keys()
  )
  model <- build_schema_model(metadata)
  expect_equal(nrow(model_tables(model)), 0L)
  expect_equal(nrow(model_dependencies(model)), 0L)
})
