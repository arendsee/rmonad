#' Convert an rmonad object to a list of monads
#'
#' @param x An Rmonad object
#' @param recurse_nests logical Should nested pipelines be included?
#' @param ... Additional arguments (unused)
#' @export
as.list.Rmonad <- function(x, recurse_nests=TRUE, ...){
  ms <- list()
  for(b in m_branch(x)){
    ms <- as.list(b, recurse_nests) %++% ms
  }
  ms <- append(list(x), ms)
  for(p in m_parents(x)){
    if(! m_id(x) %in% sapply(m_branch(p), m_id))
      ms <- as.list(p, recurse_nests) %++% ms
  }
  if(recurse_nests && has_nest(x)){
    ms <- as.list(m_nest(x), recurse_nests) %++% ms
  }
  if(x$has_prior()){
    ms <- as.list(x$get_prior()) %++% ms
  }
  unique(ms)
}

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
