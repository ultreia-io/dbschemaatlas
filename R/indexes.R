#' Generate directory indexes
#'
#' Creates a simple `index.html` in the root and each subdirectory. Existing
#' indexes are overwritten; generated links use relative paths. Asset directories
#' can be excluded from navigation and index generation.
#'
#' @param root_directory Root report directory.
#' @param title Index title.
#' @param exclude_directories Directory names to omit, including their descendants.
#' @return Paths of generated indexes, invisibly.
#' @export
generate_report_indexes <- function(root_directory, title = "Schema reports",
                                    exclude_directories = character()) {
  if (!dir.exists(root_directory)) stop("Root directory does not exist: ", root_directory, call. = FALSE)
  if (!is.character(exclude_directories) || anyNA(exclude_directories) ||
      any(!nzchar(exclude_directories)) || any(grepl("[/\\\\]", exclude_directories))) {
    stop("`exclude_directories` must contain directory names.", call. = FALSE)
  }
  directories <- unique(c(root_directory, list.dirs(root_directory, recursive = TRUE, full.names = TRUE)))
  root <- normalizePath(root_directory, winslash = "/")
  directories <- directories[vapply(directories, function(directory) {
    relative <- substring(normalizePath(directory, winslash = "/"), nchar(root) + 2L)
    !any(strsplit(relative, "/", fixed = TRUE)[[1L]] %in% exclude_directories)
  }, logical(1L))]
  indexes <- vapply(directories, function(directory) {
    children <- list.dirs(directory, recursive = FALSE, full.names = FALSE)
    children <- setdiff(children, exclude_directories)
    files <- list.files(directory, pattern = "\\.html$", full.names = FALSE)
    files <- setdiff(files, "index.html")
    items <- c(
      if (!identical(normalizePath(directory), normalizePath(root_directory))) "<li><a href='../index.html'>..</a></li>",
      sprintf("<li><a href='%s/index.html'>%s/</a></li>", utils::URLencode(children), escape_html(children)),
      sprintf("<li><a href='%s'>%s</a></li>", utils::URLencode(files), escape_html(files))
    )
    html <- sprintf("<!doctype html><html><head><meta charset='utf-8'><title>%s</title></head><body><h1>%s</h1><ul>%s</ul></body></html>",
                    escape_html(title), escape_html(title), paste(items, collapse = "\n"))
    path <- file.path(directory, "index.html")
    writeLines(html, path, useBytes = TRUE)
    path
  }, character(1L))
  invisible(indexes)
}
