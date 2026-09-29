#' Parse language-tagged descriptions
#'
#' @param x One character value containing blocks such as `[EN] Text`.
#' @return A named list of trimmed descriptions. Untagged text is returned as `EN`.
#' @export
parse_description <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(list())
  pattern <- "(?m)^[ \t]*(?:\\[[A-Z]{2}\\][ \t]*:?[ \t]*|[A-Z]{2}:[ \t]*)"
  starts <- gregexpr(pattern, x, perl = TRUE)[[1L]]
  if (starts[[1L]] < 0L) return(list(EN = trimws(x)))
  lengths <- attr(starts, "match.length")
  headers <- substring(x, starts, starts + lengths - 1L)
  languages <- sub("^[ \t]*\\[?([A-Z]{2})\\]?.*$", "\\1", headers, perl = TRUE)
  ends <- c(starts[-1L] - 1L, nchar(x))
  text <- trimws(substring(x, starts + lengths, ends))
  stats::setNames(as.list(text), languages)
}

#' Create stable HTML anchors
#'
#' @param ... Values to combine.
#' @return A lowercase HTML-safe identifier.
#' @export
atlas_anchor <- function(...) {
  value <- paste(..., sep = "_")
  value <- gsub("[^A-Za-z0-9_-]", "_", value)
  tolower(gsub("_+", "_", value))
}

escape_html <- function(x) as.character(htmltools::htmlEscape(ifelse(is.na(x), "", x)))
