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

#' Extract a monad by ID
#'
#' If the id is not found, an error is raised
#'
#' NOTE: This function will likely be deprecated and replaced with something
#' better in the near future.
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @param id a single integer ID
#' @param isolate logical, should the context of the returned monad be removed
#' @return a single Rmonad
#' @export
extract_by_id <- function(m, id, isolate=FALSE){
  stopifnot(length(id) == 1)
  .id_equal <- function(x) {
    m_id(x) == id
  }
  ms <- monad_to_list(m) %>%
    Filter(f=.id_equal)  %>%
    unique               %>%
    unlist
  x <- if(length(ms) != 1){
    as_monad(stop("id not found"))
  } else {
    ms[[1]]
  }
  if(isolate)
    x <- forget(x)
  x
}
