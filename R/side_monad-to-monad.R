#' Remove a level of nesting in an Rmonad
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @export
unnest <- function(m){
  if(is_rmonad(m) && has_value(m) && is_rmonad(m_value(m))){
    mm <<- m
    nest <- m_value(m)
    nest@graph <- igraph::set.vertex.attribute(
      graph = nest@graph,
      name  = "nest_depth",
      value = igraph::V(nest@graph)$nest_depth + (m_nest_depth(m) - m_nest_depth(nest) + 1)
    )
    m_nest(m) <- nest 
  }
  m
}

#' Add documentation to a monad
#'
#' This function is deprecated. Use a docstring instead.
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @param ... any number of strings
#' @return The input Rmonad with an added doc string
#' @export
doc <- function(m, ...){

  .Deprecated("Use docstring")

  m_doc(m) <- paste(list(...), collapse=" ")
  m
}

