#' Render an Rmonad graph
#'
#' Convert the Rmonad object to a DiagrammeR graph and then render it
#'
#' The nodes in the graph represent both a function and the function's output.
#' The edges are relationships between nodes. In an unnested pipeline, every
#' edge represents data flow from source to sink (solid black edges). Nested
#' pipelines contain three additional edge types: a transitive edge, where a
#' node is dependent on a value that was passed to its parent (dotted grey
#' line); a nest edge linking a node to the nested node that produced its value
#' (solid red line); a 'prior' edge for pipelines coupled with the \code{\%__\%}
#' operator (thick dotted blue line).
#'
#' @param x An Rmonad object
#' @param y This variable is currently ignored
#' @param label The node labels. If NULL, the node labels will equal node ids.
#' It may be one of the strings ['code', 'time', 'space', 'value', 'depth']. If
#' 'value' is selected, nodes with no value cached are represented with '-'.
#' Alternatively, it may be a function that maps a single Rmonad object to a
#' string.
#' @param color How to color the nodes. Default is 'status', which colors green
#' for passing, orange for warning, and red for error. Alternatively, color can
#' be a function of an Rmonad object, which will be applied to each node.
#' @param ... Additional arguments (unused currently)
#' @export
plot.Rmonad <- function(x, y, label=NULL, color='status', ...){
  stop("NOT IMPLEMENTED")
}
