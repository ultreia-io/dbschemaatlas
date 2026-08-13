#' Create dependency graph data
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
  group_for_schema <- function(schema) {
    groups <- names(config$schema_groups)[vapply(config$schema_groups, function(values) schema %in% values, logical(1L))]
    if (length(groups)) groups[[1L]] else schema
  }
  nodes <- tables[, list(id = table_id, label = table_id, group = vapply(schema, group_for_schema, character(1L)))]
  nodes[, color := vapply(group, schema_color, character(1L), config = config)]
  edges <- model$dependencies[
    table_id %in% nodes$id & target_table_id %in% nodes$id,
    list(from = table_id, to = target_table_id, label = foreign_key,
         title = paste(table_id, "depends on", target_table_id))
  ]
  list(nodes = nodes, edges = edges)
}

#' Create an interactive dependency graph
#'
#' @inheritParams dependency_graph_data
#' @return A `visNetwork` htmlwidget.
#' @export
dependency_graph <- function(model, config = atlas_report_config()) {
  data <- dependency_graph_data(model, config)
  graph <- visNetwork::visNetwork(data$nodes, data$edges, width = "100%", height = "600px")
  graph <- visNetwork::visNodes(graph, shape = "dot", size = 18, borderWidthSelected = 4)
  graph <- visNetwork::visEdges(graph, arrows = "to", smooth = TRUE)
  graph <- visNetwork::visPhysics(graph, stabilization = TRUE)
  graph <- visNetwork::visInteraction(graph, navigationButtons = TRUE, dragNodes = TRUE, zoomView = TRUE)
  script <- config$template_overrides$graph_script
  if (!is.null(script)) {
    if (!file.exists(script)) stop("Graph script override not found: ", script, call. = FALSE)
    graph <- htmlwidgets::onRender(graph, paste(readLines(script, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  graph
}
