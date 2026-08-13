render_markdown_description <- function(description) {
  values <- parse_description(description)
  if (!length(values)) return("*Not provided*")
  paste(vapply(names(values), function(language) {
    sprintf("**%s:** %s", escape_html(language), escape_html(values[[language]]))
  }, character(1L)), collapse = "<br>")
}

render_markdown_table <- function(data, raw_columns = character()) {
  if (!nrow(data)) return("*None.*")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  for (name in names(data)) {
    if (is.list(data[[name]])) data[[name]] <- flatten_list_column(data[[name]])
    if (!name %in% raw_columns) data[[name]] <- vapply(data[[name]], escape_html, character(1L))
  }
  header <- paste(names(data), collapse = " | ")
  separator <- paste(rep("---", ncol(data)), collapse = " | ")
  rows <- apply(data, 1L, paste, collapse = " | ")
  paste(c(paste0("| ", header, " |"), paste0("| ", separator, " |"), paste0("| ", rows, " |")), collapse = "\n")
}

build_report_body <- function(model, config, timestamp) {
  schemas <- model$schemas
  if (!is.null(config$schemas)) schemas <- schemas[schema %in% config$schemas]
  sections <- list()
  for (schema_name in schemas$schema) {
    schema <- schemas[schema == schema_name]
    schema_label <- if (schema_name %in% names(config$domain_labels)) {
      paste0(config$domain_labels[[schema_name]], " (", schema_name, ")")
    } else schema_name
    parts <- c(
      sprintf("## %s {#schema_%s}", escape_html(schema_label), atlas_anchor(schema_name)),
      render_markdown_description(schema$description[[1L]])
    )
    tables <- model$tables[schema == schema_name]
    for (row in seq_len(nrow(tables))) {
      table_name <- tables$table[[row]]
      id <- tables$table_id[[row]]
      table_parts <- c(
        sprintf("### %s {#table_%s}", escape_html(table_name), atlas_anchor(schema_name, table_name)),
        render_markdown_description(tables$description[[row]])
      )
      if (config$sections[["columns"]]) {
        value <- model$columns[schema == schema_name & table == table_name,
          list(column, type, mandatory, primary_key, foreign_key, description)]
        table_parts <- c(table_parts, "#### Columns", render_markdown_table(value))
      }
      if (config$sections[["dependencies"]]) {
        value <- model$dependencies[table_id == id, list(
          columns,
          type = vapply(target_table_id, config$link_type, character(1L)),
          target = mapply(config$link_url, target_schema, target_table, target_table_id, USE.NAMES = FALSE),
          target_columns,
          foreign_key
        )]
        table_parts <- c(table_parts, "#### Dependencies", render_markdown_table(value, "target"))
      }
      if (config$sections[["usages"]]) {
        value <- model$usages[table_id == id, list(
          columns,
          type = vapply(usage_table_id, config$link_type, character(1L)),
          usage = mapply(config$link_url, usage_schema, usage_table, usage_table_id, USE.NAMES = FALSE),
          usage_columns,
          usage_foreign_key
        )]
        table_parts <- c(table_parts, "#### Usages", render_markdown_table(value, "usage"))
      }
      parts <- c(parts, table_parts)
    }
    sections[[schema_name]] <- paste(parts, collapse = "\n\n")
  }
  paste(unlist(sections, use.names = FALSE), collapse = "\n\n")
}

copy_optional_asset <- function(path, output_directory) {
  if (is.null(path)) return("")
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) stop("Optional asset not found: ", path, call. = FALSE)
  destination <- file.path(output_directory, basename(path))
  file.copy(path, destination, overwrite = TRUE)
  normalizePath(destination, winslash = "/", mustWork = TRUE)
}

#' Render a complete database schema report
#'
#' @param model A `dbschema_model`.
#' @param config An [atlas_report_config()] object.
#' @param timestamp Timestamp used in the report and filename.
#' @param quiet Passed to [rmarkdown::render()].
#' @return The normalized report path, invisibly.
#' @export
render_schema_report <- function(model, config = atlas_report_config(), timestamp = Sys.time(), quiet = TRUE) {
  model <- validate_model(model)
  config <- validate_config(config)
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Package `rmarkdown` is required to render reports.", call. = FALSE)
  dir.create(config$output_directory, recursive = TRUE, showWarnings = FALSE)
  formatted_timestamp <- if (identical(config$timestamp_format, "")) "" else format(timestamp, config$timestamp_format)
  timestamp_suffix <- if (nzchar(formatted_timestamp)) paste0("_", gsub("[^A-Za-z0-9_-]", "_", formatted_timestamp)) else ""
  output_file <- paste0(config$filename_prefix, timestamp_suffix, ".html")
  input <- tempfile(fileext = ".Rmd")
  on.exit(unlink(input), add = TRUE)
  logo <- copy_optional_asset(config$logo, config$output_directory)
  analytics <- if (is.null(config$analytics_html)) "" else {
    if (length(config$analytics_html) == 1L && file.exists(config$analytics_html)) {
      paste(readLines(config$analytics_html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else paste(config$analytics_html, collapse = "\n")
  }
  style <- paste(readLines(template_path(config, "style", "atlas.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  template <- paste(readLines(template_path(config, "report", "report.Rmd.tpl"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rendered <- whisker::whisker.render(template, list(
    title = config$title,
    abstract = config$abstract,
    author = if (is.null(config$author)) "" else config$author,
    timestamp = formatted_timestamp,
    logo = logo,
    has_logo = nzchar(logo),
    analytics = analytics,
    style = style,
    body = build_report_body(model, config, timestamp)
  ))
  writeLines(rendered, input, useBytes = TRUE)
  result <- rmarkdown::render(input, output_file = output_file, output_dir = config$output_directory,
                              envir = new.env(parent = baseenv()), quiet = quiet)
  if (config$sections[["graphs"]]) {
    graph_file <- file.path(config$output_directory, paste0(config$filename_prefix, "_dependencies.html"))
    htmlwidgets::saveWidget(dependency_graph(model, config), graph_file, selfcontained = TRUE)
  }
  invisible(normalizePath(result, winslash = "/", mustWork = TRUE))
}
