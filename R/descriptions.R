#' Parse language-tagged descriptions
#'
#' @param x One character value containing blocks such as `[EN] Text`.
#' @return A named list of trimmed descriptions. Untagged text is returned as `EN`.
#' @export
parse_description <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(list())
  pattern <- "\\[([A-Z]{2})\\]\\s*([\\s\\S]*?)(?=\\n\\s*\\[[A-Z]{2}\\]|$)"
  match <- gregexpr(pattern, x, perl = TRUE)
  values <- regmatches(x, match)[[1L]]
  if (!length(values)) return(list(EN = trimws(x)))
  languages <- sub("^\\[([A-Z]{2})\\].*$", "\\1", values)
  text <- trimws(sub("^\\[[A-Z]{2}\\]\\s*", "", values, perl = TRUE))
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
