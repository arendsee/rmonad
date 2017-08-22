.scat <- function(s, ...) cat(sprintf(s, ...)) 

.print_record <- function(x, verbose=FALSE, print_value=TRUE, ...) {

  if(has_doc(x)){
    .scat("\n\n    %s\n\n", m_doc(x))
  }
  .scat('R> "%s"', paste(m_code(x), collapse="\n"))

  if(verbose && (has_time(x) || has_mem(x))){
    cat("\n  ")
    if(has_mem(x))  { .scat(" size: %s", m_mem(x))  }
    if(has_time(x)) { .scat(" time: %s", m_time(x)) }
  }
  if(has_error(x)){
    .scat("\n * ERROR: %s", m_error(x))
  }
  if(has_warnings(x)){
    .scat("\n * WARNING: %s",
      paste(m_warnings(x), collapse="\n * WARNING: ")
    )
  }
  if(has_notes(x)){
    .scat("\n * NOTE: %s",
      paste(m_notes(x), collapse="\n * NOTE: ")
    )
  }
  if(has_branch(x)){
    .scat("\nHas %s branches", length(has_branch(x)))
  }
  if(has_value(x) && print_value){
    cat("\n")
    print(m_value(x))
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

  ms <- as.list(x)

  for(i in seq_len(length(ms)-1)){
    .print_record(ms[[i]], print_value=print_value)
  }
  .print_record(x, print_value=FALSE)

  if(length(ms) > 1){
    cat("\n ----------------- \n\n")
  }

  if(print_value)
    print(m_value(x))

  if(!m_OK(x)){
    cat(" *** FAILURE *** \n")
  }
}

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
  unique(ms)
}

#' Render an Rmonad graph
#'
#' Convert the Rmonad object to a DiagrammeR graph and then render it
#'
#' The nodes in the graph represent both a function and the function's output.
#' The edges are relationships between nodes. In an unnested pipeline, every
#' edge represents data flow from source to sink (solid black edges). Nested
#' pipelines contain two additional edge types: a transitive edge, where a node
#' is dependent on a value that was passed to its parent (dotted grey line);
#' and a nest edge linking a node to the nested node that produced its value
#' (solid red line).
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

  y <- NULL

  label <-
  if(is.function(label)) {
    label               
  } else if(is.null(label)){
    m_id
  } else if(label == "code"){
    function(m){
      if(has_code(m)){
        paste0(m_code(m), collapse="\n")
      } else {
        "."
      }
    }
  } else if(label == "time") {
    m_time
  } else if (label == "space") {
    m_mem
  } else if (label == "depth") {
    m_nest_depth
  } else if (label == "value") {
    function(m) {
      if(has_value(m)){
        m_value(m)
      } else {
        "-"
      }
    }
  } else {
    stop("Something is wrong with the 'label' field")
  }

  color <-
  if(is.function(color)){
    color
  } else if(color == 'status'){
    function(x) {
      if(has_error(x)){
        'red'
      } else if(has_warnings(x)){
        'orange'
      } else {
        'palegreen'
      }
    }
  } else {
    stop("The 'color' field in plot.Rmonad must be either 'status' or a function")
  }

  g <- as_dgr_graph(x, label=label, color=color)

  g$edges_df$color <- ifelse(g$edges_df$rel == 'depend', 'black', 'red')
  g$edges_df$color <- ifelse(g$edges_df$rel == 'transitive', 'gray', g$edges_df$color)

  g$edges_df$style <- ifelse(g$edges_df$rel == 'transitive', "dotted", "")

  DiagrammeR::render_graph(g)
}
