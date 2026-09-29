#' Create dependency graph data
#'
#' With a configured entry point, traverse dependencies and reverse usages as in
#' a rooted graph, retaining direction-specific edge colours and line styles.
#'
#' @param model A `dbschema_model`.
#' @param config An [atlas_report_config()] object.
#' @return A list with `nodes` and `edges` data tables.
#' @export
dependency_graph_data <- function(model, config = atlas_report_config()) {
  model <- validate_model(model)
  config <- validate_config(config)
  tables <- model$tables
  if (!is.null(config$schemas)) tables <- tables[schema %in% config$schemas]
  dependencies <- model$dependencies[
    table_id %in% tables$table_id & target_table_id %in% tables$table_id
  ]
  dependencies <- data.table::copy(dependencies)
  dependencies[, relation := "\u2192"]
  root <- config$graph_entry_point
  if (!is.null(root)) {
    root <- normalize_entry_point(root)
    if (!root %in% tables$table_id) stop("Unknown graph entry point: ", root, call. = FALSE)
    visited <- character()
    traversed <- list()
    walk <- function(current) {
      if (current %in% visited || current %in% config$graph_standalone_tables) return(invisible(NULL))
      visited <<- c(visited, current)
      reverse <- dependencies[target_table_id == current & !table_id %in% visited]
      for (i in seq_len(nrow(reverse))) {
        edge <- data.table::copy(reverse[i])
        edge[, relation := "\u2190"]
        traversed[[length(traversed) + 1L]] <<- edge
        walk(edge$table_id[[1L]])
      }
      forward <- dependencies[table_id == current & !target_table_id %in% visited]
      for (i in seq_len(nrow(forward))) {
        traversed[[length(traversed) + 1L]] <<- forward[i]
        walk(forward$target_table_id[[i]])
      }
      invisible(NULL)
    }
    walk(root)
    dependencies <- if (length(traversed)) data.table::rbindlist(traversed) else dependencies[0]
    ids <- unique(c(root, dependencies$table_id, dependencies$target_table_id))
    tables <- tables[table_id %in% ids]
  }
  group_for_schema <- function(schema) {
    groups <- names(config$schema_groups)[vapply(config$schema_groups, function(values) schema %in% values, logical(1L))]
    if (length(groups)) groups[[1L]] else schema
  }
  nodes <- tables[, list(id = table_id, label = table_id,
                         group = vapply(schema, group_for_schema, character(1L)))]
  nodes[, color := vapply(tables$schema, schema_color, character(1L), config = config)]
  if (!is.null(root)) nodes[id == root, color := "red"]
  edges <- dependencies[, list(
    from = table_id, to = target_table_id,
    from_column = columns, to_column = target_columns, relation,
    title = paste0(table_id, ".", vapply(columns, paste, character(1L), collapse = ", "),
                   " \u2192 ", target_table_id, ".",
                   vapply(target_columns, paste, character(1L), collapse = ", ")),
    color = ifelse(relation == "\u2190", "#FF7F0E", "#1F77B4"),
    dashes = relation == "\u2190"
  )]
  list(nodes = nodes, edges = edges)
}

# Convert model records to the row-oriented payload used by the graph details panel.
graph_table_details <- function(model, config, ids) {
  rows <- function(data) lapply(seq_len(nrow(data)), function(i) as.list(data[i]))
  flags <- function(schema_name, table_name, names, flag) {
    columns <- model$columns[schema == schema_name & table == table_name]
    unname(columns[[flag]][match(names, columns$column)])
  }
  link <- function(schema_name, table_name, id) {
    value <- config$link_url(schema_name, table_name, id)
    if (grepl("href=['\"]#", value)) {
      sprintf('<a href="#%s">%s</a>', escape_html(id), escape_html(id))
    } else sub("<a ", "<a target=\"_iotc_code_lists\" ", value, fixed = TRUE)
  }
  result <- list(columns = list(), dependencies = list(), usages = list(), descriptions = list())
  for (id in ids) {
    table_row <- model$tables[table_id == id]
    schema_name <- table_row$schema[[1L]]
    table_name <- table_row$table[[1L]]
    columns <- model$columns[schema == schema_name & table == table_name,
                             list(column, type, mandatory, primary_key, foreign_key, description)]
    result$columns[[id]] <- rows(columns)
    result$descriptions[[id]] <- table_row$description[[1L]]
    deps <- model$dependencies[table_id == id]
    result$dependencies[[id]] <- lapply(seq_len(nrow(deps)), function(i) {
      d <- deps[i]
      list(columns = d$columns[[1L]],
           mandatory = flags(schema_name, table_name, d$columns[[1L]], "mandatory"),
           primary_key = flags(schema_name, table_name, d$columns[[1L]], "primary_key"),
           dependency_type = config$link_type(d$target_table_id[[1L]]),
           dependency_table = link(d$target_schema[[1L]], d$target_table[[1L]], d$target_table_id[[1L]]),
           dependency_columns = d$target_columns[[1L]],
           dependency_mandatory = flags(d$target_schema[[1L]], d$target_table[[1L]], d$target_columns[[1L]], "mandatory"),
           dependency_primary_key = flags(d$target_schema[[1L]], d$target_table[[1L]], d$target_columns[[1L]], "primary_key"))
    })
    usages <- model$usages[table_id == id & usage_table_id %in% ids]
    result$usages[[id]] <- lapply(seq_len(nrow(usages)), function(i) {
      d <- usages[i]
      list(columns = d$columns[[1L]],
           mandatory = flags(schema_name, table_name, d$columns[[1L]], "mandatory"),
           primary_key = flags(schema_name, table_name, d$columns[[1L]], "primary_key"),
           usage_type = config$link_type(d$usage_table_id[[1L]]),
           usage_table = link(d$usage_schema[[1L]], d$usage_table[[1L]], d$usage_table_id[[1L]]),
           usage_columns = d$usage_columns[[1L]],
           usage_mandatory = flags(d$usage_schema[[1L]], d$usage_table[[1L]], d$usage_columns[[1L]], "mandatory"),
           usage_primary_key = flags(d$usage_schema[[1L]], d$usage_table[[1L]], d$usage_columns[[1L]], "primary_key"))
    })
  }
  result
}

# Native disclosure legend; colours come from the same configuration as the nodes.
graph_legend <- function(config, schemas) {
  tags <- htmltools::tags
  node_items <- lapply(schemas, function(schema) {
    label <- unname(config$domain_labels[schema])
    if (!length(label) || is.na(label)) label <- schema
    tags$li(tags$span(class = "legend-node", style = paste0("background-color:", schema_color(schema, config))), label)
  })
  tags$details(class = "graph-legend",
    tags$summary(title = "Legend (click to pin)", `aria-label` = "Legend", `aria-expanded` = "false", "?"),
    tags$div(class = "graph-legend-content",
      tags$strong("Column icons"),
      tags$ul(
        tags$li(tags$span(class = "mandatory-icon"), "Mandatory"),
        tags$li(tags$span(class = "pk-icon"), "Primary key"),
        tags$li(tags$span(class = "fk-icon"), "Foreign key")),
      tags$strong("Nodes"),
      tags$ul(
        if (!is.null(config$graph_entry_point)) tags$li(tags$span(class = "legend-node", style = "background-color:red"), "Entry point"),
        node_items),
      tags$strong("Relationships"),
      tags$ul(
        tags$li(tags$span(class = "legend-edge dependency"), "Dependency"),
        tags$li(tags$span(class = "legend-edge usage"), "Usage (reverse lookup)"))))
}

#' Create an interactive dependency graph
#'
#' Includes a two-panel layout, node navigation, neighbourhood highlighting,
#' and a details panel showing descriptions, column flags, dependencies and usages.
#'
#' @inheritParams dependency_graph_data
#' @return A `visNetwork` htmlwidget with its details panel and embedded presentation assets.
#' @export
dependency_graph <- function(model, config = atlas_report_config()) {
  data <- dependency_graph_data(model, config)
  graph <- visNetwork::visNetwork(data$nodes, data$edges, width = "100%", height = "85vh")
  graph <- visNetwork::visNodes(graph, shape = "dot", size = 20, borderWidth = 1,
                               borderWidthSelected = 5,
                               color = list(border = "#A0A0A0", highlight = list(border = "#222222")))
  graph <- visNetwork::visEdges(graph, arrows = list(to = list(enabled = TRUE)), length = 200,
                               smooth = TRUE, font = list(align = "bottom", size = 14))
  graph <- visNetwork::visPhysics(graph, solver = "barnesHut",
    stabilization = list(enabled = TRUE, iterations = 1000),
    barnesHut = list(gravitationalConstant = -8000, springLength = 180,
                    springConstant = 0.04, damping = 0.09))
  graph <- visNetwork::visInteraction(graph, navigationButtons = TRUE, dragNodes = TRUE, zoomView = TRUE)
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    graph$dependencies <- c(graph$dependencies, list(rmarkdown::html_dependency_bootstrap("flatly")))
  }
  graph <- visNetwork::visEvents(graph,
    afterDrawing = "function() { window.myNetwork = this; }",
    selectNode = "function(properties) { if (properties.nodes.length) window.location.hash = properties.nodes[0]; }",
    deselectNode = "function() { window.location.hash = ''; }")
  style <- paste(readLines(template_path(config, "graph_style", "graph.css"),
                           warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  graph <- htmlwidgets::prependContent(graph,
    htmltools::tags$style(htmltools::HTML(style)),
    htmltools::tags$header(class = "atlas-graph-header",
      htmltools::tags$h1(class = "title", config$title),
      htmltools::tags$h3(class = "subtitle", paste("Last updated:", format(Sys.time(), "%d %B %Y %H:%M %Z"))),
      if (!is.null(config$author)) htmltools::tags$h4(class = "author", config$author)))
  legend_schemas <- unique(model$tables[table_id %in% data$nodes$id, schema])
  graph <- htmlwidgets::appendContent(graph,
    graph_legend(config, legend_schemas),
    htmltools::tags$div(class = "details-panel",
      htmltools::tags$h2("Table details"),
      htmltools::tags$div(id = "table-details", class = "details",
        htmltools::tags$p("Select a node in the graph."))))
  script <- template_path(config, "graph_script", "graph.js")
  if (!file.exists(script)) stop("Graph script not found: ", script, call. = FALSE)
  htmlwidgets::onRender(graph, paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                        data = graph_table_details(model, config, data$nodes$id))
}
