#' dbschemaatlas: reusable database schema atlases
#'
#' Extract PostgreSQL metadata through caller-owned connection providers,
#' save and load snapshots, inspect dependencies, and render neutral reports.
#' The package uses [dbiutils::db_query()],
#' [dbiutils::with_db_connection()], and [dbiutils::read_sql_file()] for its
#' low-level DBI work and does not import a database driver.
#'
#' @import data.table
#' @keywords internal
"_PACKAGE"
