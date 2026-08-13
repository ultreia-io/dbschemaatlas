test_that("report configuration is neutral and customizable", {
  config <- atlas_report_config(
    title = "Inventory", schemas = c("public", "audit"),
    schema_groups = list(core = "public"),
    schema_colors = c(public = "red"), filename_prefix = "inventory",
    domain_labels = c(public = "Core")
  )
  expect_s3_class(config, "atlas_report_config")
  expect_equal(config$title, "Inventory")
  expect_equal(dbschemaatlas:::schema_color("public", config), "red")
  expect_equal(dbschemaatlas:::schema_color("audit", config), "#4C78A8")
  expect_false(any(grepl("ROS|IOTC|FAO", unlist(config[c("title", "abstract", "filename_prefix")]), ignore.case = TRUE)))
  expect_error(atlas_report_config(title = ""), "Invalid")
  expect_s3_class(atlas_report_config(timestamp_format = ""), "atlas_report_config")
  expect_error(atlas_report_config(sections = c(columns = TRUE)), "must name")
})

test_that("link generation escapes labels and creates stable targets", {
  config <- atlas_report_config()
  link <- config$link_url("odd schema", "table", "<unsafe>")
  expect_match(link, "#table_odd_schema_table", fixed = TRUE)
  expect_match(link, "&lt;unsafe&gt;", fixed = TRUE)
})
