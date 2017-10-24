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

.scat <- function(s, ...) cat(sprintf(s, ...)) 

.print_record <- function(x, i, verbose=FALSE, print_value=TRUE) {

  if(has_doc(x, i)){
    .scat("\n\n    %s\n\n", m_doc(x, i))
  }
  .scat('R> "%s"', paste(m_code(x, i), collapse="\n"))

  if(verbose && (has_time(x, i) || has_mem(x, i))){
    cat("\n  ")
    if(has_mem(x, i))  { .scat(" size: %s", m_mem(x, i))  }
    if(has_time(x, i)) { .scat(" time: %s", m_time(x, i)) }
  }
  if(has_error(x, i)){
    .scat("\n * ERROR: %s", m_error(x, i))
  }
  if(has_warnings(x, i)){
    .scat("\n * WARNING: %s",
      paste(m_warnings(x, i), collapse="\n * WARNING: ")
    )
  }
  if(has_notes(x, i)){
    .scat("\n * NOTE: %s",
      paste(m_notes(x, i), collapse="\n * NOTE: ")
    )
  }
  if(has_children(x, i)){
    .scat("\nHas %s branches", length(m_children(x, i)))
  }
  if(has_value(x, i) && print_value){
    cat("\n")
    print(m_value(x, i))
  }
  cat("\n")
}

#' Rmonad print generic function
#'
#' @param x An Rmonad object
#' @param verbose logical print verbose output (include benchmarking)
#' @param print_value logical print the value wrapped in the Rmonad
#' @param ... Additional arguments (unused)
#' @export
print.Rmonad <- function(x, verbose=FALSE, print_value=TRUE, ...){

  for(i in seq_len(igraph::vcount(x@graph)-1)){
    .print_record(x, i, print_value=print_value)
  }
  .print_record(x, igraph::vcount(x@graph), print_value=FALSE)

  if(length(ms_id(x)) > 1){
    cat("\n ----------------- \n\n")
  }

  if(print_value)
    print(m_value(x))

  if(!m_OK(x)){
    cat(" *** FAILURE *** \n")
  }
}
setMethod("show", "Rmonad",
  function(object) print(object)
)
