test_that("multilingual and untagged descriptions are parsed", {
  expect_equal(parse_description("Plain text"), list(EN = "Plain text"))
  value <- parse_description("[EN] Hello\n\n[FR] Bonjour")
  expect_equal(value$EN, "Hello")
  expect_equal(value$FR, "Bonjour")
  expect_equal(parse_description(NA_character_), list())
})

test_that("anchors and rendered report content escape HTML", {
  expect_equal(atlas_anchor("My schema", "x.y"), "my_schema_x_y")
  expect_match(dbschemaatlas:::escape_html("<script>"), "&lt;script&gt;", fixed = TRUE)
  body <- dbschemaatlas:::build_report_body(build_schema_model(fixture_metadata()), atlas_report_config(), Sys.time())
  expect_false(grepl("<script>", body, fixed = TRUE))
})
