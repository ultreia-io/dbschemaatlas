test_that("offline report generation and indexing work end to end", {
  skip_if_not_installed("rmarkdown")
  output <- withr::local_tempdir()
  config <- atlas_report_config(
    title = "Fixture atlas <safe>", output_directory = output,
    filename_prefix = "fixture", timestamp_format = "",
    sections = c(columns = TRUE, dependencies = TRUE, usages = TRUE, graphs = FALSE)
  )
  report <- render_schema_report(build_schema_model(fixture_metadata()), config,
                                 timestamp = as.POSIXct("2026-01-02 03:04:05", tz = "UTC"))
  expect_true(file.exists(report))
  expect_equal(basename(report), "fixture.html")
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "Fixture atlas")
  expect_match(html, "public.account", fixed = TRUE)
  indexes <- generate_report_indexes(output, "Fixture reports")
  expect_true(all(file.exists(indexes)))
  expect_match(paste(readLines(file.path(output, "index.html")), collapse = ""), basename(report), fixed = TRUE)
})

test_that("template overrides are honored", {
  template <- withr::local_tempfile(fileext = ".Rmd")
  style <- withr::local_tempfile(fileext = ".css")
  writeLines(c("---", "title: '{{title}}'", "output: html_document", "---", "CUSTOM", "{{{body}}}"), template)
  writeLines("body { color: black; }", style)
  config <- atlas_report_config(template_overrides = list(report = template, style = style))
  expect_equal(dbschemaatlas:::template_path(config, "report", "report.Rmd.tpl"), template)
})

test_that("index generation validates roots and handles subdirectories", {
  expect_error(generate_report_indexes(file.path(tempdir(), "missing-atlas-root")), "does not exist")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "child"))
  paths <- generate_report_indexes(root)
  expect_length(paths, 2L)
})
