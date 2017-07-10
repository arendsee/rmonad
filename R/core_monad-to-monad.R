#' Clear an rmonad's parents
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @return The input Rmonad with all parents erased
#' @export 
forget <- function(m){
  m_parents(m) <- list()
  m
}

#' Add documentation to a monad
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @param ... any number of strings
#' @return The input Rmonad with an added doc string
#' @export
doc <- function(m, ...){
  m_doc(m) <- paste(list(...), collapse=" ")
  m
}
