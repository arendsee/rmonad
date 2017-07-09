#' Store a monad's value
#'
#' @param m An Rmonad
#' @return The input Rmonad with its value tagged for storage
#' @keywords internal
#' @family monad-to-monad
#' @export 
store <- function(m){
  .store(m)
}

#' Wipe a monad's history
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @return The input Rmonad with history erased
#' @export 
forget <- function(m){
  m_history(m) <- list()
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

