#' Interactive shiny interface to create and edit networks
#'
#' @return
#' @export
#'

networkCreatoR <- function() {
  require(shiny)
  require(visNetwork)
  library(dplyr)

  # Initialize the graph with these nodes/edges.  We have to assign edges an ID
  # in case the user edits them later.
  init.nodes.df = data.frame(id = c("foo", "bar"),
                             label = c("Foo", "Bar"),
                             group = c(NA, NA) |> as.character(),
                             value = 1,
                             color = c("red", "darkgreen"),
                             stringsAsFactors = F)
  init.edges.df = data.frame(id = "foobar",
                             from = "foo",
                             to = "bar",
                             width = 1,
                             type = NA |> as.character(),
                             label = NA |> as.character(),
                             stringsAsFactors = F)

  ui <- fluidPage(
    fluidRow(
      # Display two tables: one with the nodes, one with the edges.
      column(
        width = 5,
        tags$h1("Nodes:"),
        tableOutput("all_nodes"),
        tags$h1("Edges:"),
        tableOutput("all_edges"),
        downloadButton("downloadData_nodes_csv", "Download nodes (CSV)"),
        downloadButton("downloadData_nodes_rds", "Download nodes (RDS)"),
        downloadButton("downloadData_edges_csv", "Download edges (CSV)"),
        downloadButton("downloadData_edges_rds", "Download edges (RDS)"),
        fileInput("upload_nodes", label = "Upload nodes...", accept = c(".csv", ".rds")),
        tableOutput("uploaded_nodes"),
        fileInput("upload_edges", label = "Upload edges...", accept = c(".csv", ".rds")),
        tableOutput("uploaded_edges"),
        actionButton("load_premade_network", "Load imported premade network")

      ),
      # The graph.
      column(
        width = 6,
        visNetworkOutput("editable_network", height = "800px")
      )
    )
  )

  server <- function(input, output) {

    # `graph_data` is a list of two data frames: one of nodes, one of edges.
    graph_data = reactiveValues(
      nodes = init.nodes.df,
      edges = init.edges.df
    )

    # Render the graph.
    output$editable_network <- renderVisNetwork({
      visNetwork(graph_data$nodes, graph_data$edges) %>%
        visOptions(manipulation = list(enabled = TRUE,
                                       editNodeCols = c("id", "label", "group", "color", "value"),
                                       editEdgeCols = c("id", "label", "width", "type"),
                                       addNodeCols = c("id", "label", "group", "value", "color")))
    })

    # If the user edits the graph, this shows up in
    # `input$[name_of_the_graph_output]_graphChange`.  This is a list whose
    # members depend on whether the user added a node or an edge.  The "cmd"
    # element tells us what the user did.
    observeEvent(input$editable_network_graphChange, {

      # If the user added a node, add it to the data frame of nodes.
      if(input$editable_network_graphChange$cmd == "addNode") {
        temp = bind_rows(
          graph_data$nodes,
          data.frame(id = input$editable_network_graphChange$id,
                     label = input$editable_network_graphChange$label |> is.null() |> ifelse(NA, input$editable_network_graphChange$label),
                     group = input$editable_network_graphChange$group |> is.null() |> ifelse(NA, input$editable_network_graphChange$group),
                     value = input$editable_network_graphChange$value |> is.null() |> ifelse(1, input$editable_network_graphChange$value),
                     color = input$editable_network_graphChange$color |> is.null() |> ifelse("red", input$editable_network_graphChange$color),
                     stringsAsFactors = F)
        )
        graph_data$nodes = temp
      }

      # If the user added an edge, add it to the data frame of edges.
      else if(input$editable_network_graphChange$cmd == "addEdge") {
        temp = bind_rows(
          graph_data$edges,
          data.frame(id = input$editable_network_graphChange$id,
                     from = input$editable_network_graphChange$from,
                     to = input$editable_network_graphChange$to,
                     width = 1,
                     type = NA,
                     stringsAsFactors = F)
        )
        graph_data$edges = temp
      }

      # If the user edited a node, update that record.
      else if(input$editable_network_graphChange$cmd == "editNode") {
        temp <- graph_data$nodes

        temp$label[temp$id == input$editable_network_graphChange$id] <-
          input$editable_network_graphChange$label |>
          is.null() |> ifelse(temp$label[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$label |> as.character())

        temp$group[temp$id == input$editable_network_graphChange$id] <-
          input$editable_network_graphChange$group |>
          is.null() |> ifelse(temp$group[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$group |> as.character())

        temp$value[temp$id == input$editable_network_graphChange$id] <-
          input$editable_network_graphChange$value |>
          is.null() |> ifelse(temp$value[temp$id == input$editable_network_graphChange$id] |> as.numeric(),
                              input$editable_network_graphChange$value |> as.numeric())

        temp$color[temp$id == input$editable_network_graphChange$id] <-
          input$editable_network_graphChange$color |>
          is.null() |> ifelse(temp$color[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$color |> as.character())

        graph_data$nodes <- temp
      }

      # If the user edited an edge, update that record.
      else if(input$editable_network_graphChange$cmd == "editEdge") {
        temp <- graph_data$edges

        temp$from[temp$id == input$editable_network_graphChange$id] <- input$editable_network_graphChange$from |>
          is.null() |> ifelse(temp$from[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$from |> as.character())

        temp$to[temp$id == input$editable_network_graphChange$id] <- input$editable_network_graphChange$to |>
          is.null() |> ifelse(temp$to[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$to |> as.character())

        temp$width[temp$id == input$editable_network_graphChange$id] <- input$editable_network_graphChange$width |>
          is.null() |> ifelse(temp$width[temp$id == input$editable_network_graphChange$id] |> as.numeric(),
                              input$editable_network_graphChange$width |> as.numeric())

        temp$label[temp$id == input$editable_network_graphChange$id] <- input$editable_network_graphChange$label |>
          is.null() |> ifelse(temp$label[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$label |> as.character())

        temp$type[temp$id == input$editable_network_graphChange$id] <- input$editable_network_graphChange$type |>
          is.null() |> ifelse(temp$type[temp$id == input$editable_network_graphChange$id] |> as.character(),
                              input$editable_network_graphChange$type |> as.character())
        graph_data$edges = temp
      }

      # If the user deleted something, remove those records.
      else if(input$editable_network_graphChange$cmd == "deleteElements") {
        for(node.id in input$editable_network_graphChange$nodes) {
          temp = graph_data$nodes
          temp = temp[temp$id != node.id,]
          graph_data$nodes = temp
        }
        for(edge.id in input$editable_network_graphChange$edges) {
          temp = graph_data$edges
          temp = temp[temp$id != edge.id,]
          graph_data$edges = temp
        }
      }
    })

    # Render the table showing all the nodes in the graph.
    output$all_nodes = renderTable({
      graph_data$nodes[, c("id,", "label", "group", "value")]
    })

    # Render the table showing all the edges in the graph.
    output$all_edges = renderTable({
      graph_data$edges[, c("from", "to", "type", "label")]
    })



    output$downloadData_nodes_csv <- downloadHandler(
      filename = "nodes.csv",
      content = function(file) {
        write.csv2(graph_data$nodes, file, row.names = FALSE)
      }
    )
    output$downloadData_nodes_rds <- downloadHandler(
      filename = "nodes.rds",
      content = function(file) {
        saveRDS(graph_data$nodes, file)
      }
    )

    output$downloadData_edges_csv <- downloadHandler(
      filename = "edges.csv",
      content = function(file) {
        write.csv2(graph_data$edges, file, row.names = FALSE)
      }
    )
    output$downloadData_edges_rds <- downloadHandler(
      filename = "edges.rds",
      content = function(file) {
        saveRDS(graph_data$edges, file)
      }
    )

    # Print table of uploaded nodes
    output$uploaded_nodes <-
      renderTable({
        if(!is.null(input$upload_nodes$datapath)) {
          if (tools::file_ext(input$upload_nodes$datapath) == "rds") {readRDS(input$upload_nodes$datapath)}
          if (tools::file_ext(input$upload_nodes$datapath) == "csv") {readr::read_csv2(input$upload_nodes$datapath, show_col_types = FALSE)}
        }
      }
      )

    # Print table of uploaded edges
    output$uploaded_edges <-
      renderTable({
        if(!is.null(input$upload_edges$datapath)) {
          if (tools::file_ext(input$upload_edges$datapath) == "rds") {readRDS(input$upload_edges$datapath)}
          if (tools::file_ext(input$upload_edges$datapath) == "csv") {readr::read_csv2(input$upload_edges$datapath, show_col_types = FALSE)}
        }})

    observeEvent(input$load_premade_network, {
      temp <- list()

      if (tools::file_ext(input$upload_nodes$datapath) == "rds") {temp$nodes <- readRDS(input$upload_nodes$datapath)}
      if (tools::file_ext(input$upload_nodes$datapath) == "csv") {temp$nodes <- readr::read_csv2(input$upload_nodes$datapath, show_col_types = FALSE)}

      if (!"value" %in% colnames(temp$nodes)) temp$nodes$value <- NA
      if (!"group" %in% colnames(temp$nodes)) temp$nodes$group <- NA
      if (!"label" %in% colnames(temp$nodes)) temp$nodes$label <- NA
      if (!"color" %in% colnames(temp$nodes)) temp$nodes$color <- NA
      if (!"id" %in% colnames(temp$nodes)) temp$nodes$id <- 1:nrow(temp$nodes)

      # On vérifie qu'il y a des edges
      if (!is.null(input$upload_edges$datapath)) {
        if (tools::file_ext(input$upload_edges$datapath) == "rds") {temp$edges <- readRDS(input$upload_edges$datapath)}
        if (tools::file_ext(input$upload_edges$datapath) == "csv") {temp$edges <- readr::read_csv2(input$upload_edges$datapath, show_col_types = FALSE)}

        if (!"value" %in% colnames(temp$edges)) temp$edges$width <- NA
        if (!"type" %in% colnames(temp$edges)) temp$edges$type <- NA
        if (!"label" %in% colnames(temp$edges)) temp$edges$label <- NA
        if (!"id" %in% colnames(temp$edges)) temp$edges$id <- 1:nrow(temp$edges)

      # Sinon, on crée une base edges vide
        } else {
          temp$edges <-
            data.frame(id = character(0),
                       from = character(0),
                       to = character(0),
                       width = numeric(0),
                       type = character(0),
                       label = character(0),
                       stringsAsFactors = F)
      }


      graph_data$nodes <- temp$nodes
      graph_data$edges <- temp$edges

      rm(temp)
    })


  }

  shinyApp(ui = ui, server = server)}
