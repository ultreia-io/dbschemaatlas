#' Configure an atlas report
#'
#' @param title Report title.
#' @param abstract Introductory text.
#' @param author Optional author.
#' @param logo Optional logo path.
#' @param analytics_html Optional analytics HTML fragment.
#' @param schemas Optional schema selection.
#' @param schema_groups Named list mapping group labels to schema vectors.
#' @param schema_colors Named character vector of schema colors.
#' @param filename_prefix Filename prefix for generated reports.
#' @param output_directory Caller-selected output directory.
#' @param link_type Function accepting `schema.table` and returning a label.
#' @param link_url Function accepting schema, table, and label.
#' @param domain_labels Named character vector of labels.
#' @param timestamp_format Format passed to [base::format()].
#' @param sections Named logical vector controlling optional report sections.
#' @param template_overrides Named list containing custom `report`, `style`, or
#'   `graph_script` paths.
#' @return An `atlas_report_config` object.
#' @export
atlas_report_config <- function(
    title = "Database schema atlas",
    abstract = "Database schema metadata, relationships, and descriptions.",
    author = NULL,
    logo = NULL,
    analytics_html = NULL,
    schemas = NULL,
    schema_groups = list(),
    schema_colors = character(),
    filename_prefix = "database_schema",
    output_directory = ".",
    link_type = function(table_id) "Database table",
    link_url = function(schema, table, label) {
      sprintf("<a href='#table_%s'>%s</a>", atlas_anchor(schema, table), escape_html(label))
    },
    domain_labels = character(),
    timestamp_format = "%Y-%m-%d_%H-%M-%S",
    sections = c(columns = TRUE, dependencies = TRUE, usages = TRUE, graphs = TRUE),
    template_overrides = list()) {
  scalar_text <- list(title = title, abstract = abstract, filename_prefix = filename_prefix,
                      output_directory = output_directory)
  invalid <- names(scalar_text)[!vapply(scalar_text, function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1L))]
  if (length(invalid)) stop("Invalid report setting(s): ", paste(invalid, collapse = ", "), call. = FALSE)
  if (!is.character(timestamp_format) || length(timestamp_format) != 1L || is.na(timestamp_format)) {
    stop("`timestamp_format` must be one character value.", call. = FALSE)
  }
  if (!is.null(schemas)) schemas <- validate_schemas(schemas)
  if (!is.function(link_type) || !is.function(link_url)) stop("Link generators must be functions.", call. = FALSE)
  section_names <- names(sections)
  sections <- stats::setNames(as.logical(sections), section_names)
  if (is.null(names(sections)) || !all(c("columns", "dependencies", "usages", "graphs") %in% names(sections))) {
    stop("`sections` must name columns, dependencies, usages, and graphs.", call. = FALSE)
  }
  result <- list(
    title = title, abstract = abstract, author = author, logo = logo,
    analytics_html = analytics_html, schemas = schemas, schema_groups = schema_groups,
    schema_colors = schema_colors, filename_prefix = filename_prefix,
    output_directory = output_directory, link_type = link_type, link_url = link_url,
    domain_labels = domain_labels, timestamp_format = timestamp_format,
    sections = sections, template_overrides = template_overrides
  )
  class(result) <- c("atlas_report_config", "list")
  result
}

validate_config <- function(config) {
  if (!inherits(config, "atlas_report_config")) stop("`config` must be created by `atlas_report_config()`.", call. = FALSE)
  config
}

schema_color <- function(schema, config) {
  if (schema %in% names(config$schema_colors)) unname(config$schema_colors[[schema]]) else "#4C78A8"
}

template_path <- function(config, name, default) {
  override <- config$template_overrides[[name]]
  if (is.null(override)) atlas_resource("templates", default) else override
}
