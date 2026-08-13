atlas_resource <- function(type, name, must_work = TRUE) {
  if (!type %in% c("sql", "templates")) {
    stop("`type` must be either 'sql' or 'templates'.", call. = FALSE)
  }
  path <- system.file(type, name, package = "dbschemaatlas", mustWork = FALSE)
  if (!nzchar(path) && identical(Sys.getenv("TESTTHAT"), "true")) {
    path <- file.path("inst", type, name)
  }
  if (must_work && (!nzchar(path) || !file.exists(path))) {
    stop("Package resource not found: ", type, "/", name, call. = FALSE)
  }
  path
}

read_atlas_sql <- function(name) {
  dbiutils::read_sql_file(atlas_resource("sql", name))
}
