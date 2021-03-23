#' @export
create_hidden_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    if (.Platform$OS.type == "windows") {
      # Invisible directory in windows
      system2(command = "attrib", args = paste("+h", path))
    }
  }
}

#' @export
get_option <- function(name, default = NULL) {
  if (is.null(get_golem_options(name))) {
    return(default)
  }

  return(golem::get_golem_options(name))
}

#' @export
alert_error <- function(message = NULL) {
  shinyalert(
    title = "Error",
    text = message,
    type = "error",
    closeOnClickOutside = TRUE
  )
}

#' @export
check_null <- function(x, default) {
  if (is.null(x)) {
    default
  } else {
    x
  }
}

# descentConfigurationTree <- function(iraceResults, configuration_id) {

#   recursiveChilds <- function(id) {
#     ids <- data.frame()
#     childs <- subset(iraceResults$allConfigurations, .PARENT. == id, select = ".ID.")

#     if (nrow(childs) == 0)
#       return(ids)

#     ids <- data.frame(from = id, to = childs$.ID.)

#     for (row in seq_len(nrow(childs))) {
#       childId <- childs[row,]
#       ids <- rbind(ids, recursiveChilds(childId))
#     }

#     return(ids)
#   }

#   return(unique(recursiveChilds(configuration_id)))
# }

# configurationTrajectory <- function(iraceResults, configuration_id) {

#   recursiveParents <- function(id) {
#     ids <- data.frame()
#     parent <- subset(iraceResults$allConfigurations, .ID. == id, select = ".PARENT.")$.PARENT.

#     if (length(parent) == 0 || is.na(parent))
#       return(ids)

#     ids <- data.frame(from = id, to = parent)
#     ids <- rbind(ids, recursiveParents(parent))

#     return(ids)
#   }

#   return(recursiveParents(configuration_id))
# }

# treePlot <- function(data, title) {
#   G <- graph_from_data_frame(data)

#   vs <- V(G)
#   es <- as.data.frame(get.edgelist(G, names = FALSE))
#   node.data <- get.data.frame(G, what = "vertices")

#   ne <- length(es[1]$V1)

#   L <- layout_as_tree(G)
#   Xn <- L[, 1]
#   Yn <- L[, 2]

#   tree <- plot_ly(
#     x = ~Xn,
#     y = ~Yn,
#     type = "scatter",
#     mode = "markers",
#     text = vs$name,
#     hoverinfo = "text",
#     marker = list(
#       color = as.factor(node.data$name),
#       size = I(50)
#     )
#   )

#   edge_shapes <- list()

#   for (i in seq_len(ne)) {
#     v0 <- es[i,]$V1
#     v1 <- es[i,]$V2

#     if (v0 == v1)
#       next

#       edge_shape <- list(
#       type = "line",
#       layer = "below",
#       line = list(color = "#030303", width = 0.3),
#       x0 = Xn[v0],
#       y0 = Yn[v0],
#       x1 = Xn[v1],
#       y1 = Yn[v1]
#     )

#     edge_shapes[[i]] <- edge_shape
#   }

#   axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

#   plot_tree <- layout(
#     tree,
#     title = title,
#     shapes = edge_shapes,
#     xaxis = axis,
#     yaxis = axis,
#     showlegend = FALSE
#   )

#   return(plot_tree)
# }
