#' Remove a level of nesting in an Rmonad
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @export
unnest <- function(m){
  mm <<- m
  if(is_rmonad(m) && has_value(m) && is_rmonad(m_value(m))){
    m_nest(m) <- m_value(m)
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

