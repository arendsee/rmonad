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

#' Remove a level of nesting in an Rmonad
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @export
unnest <- function(m){
  if(is_rmonad(m) && has_value(m) && is_rmonad(m_value(m))){
    m_nest(m) <- m_value(m)$clone()
    m_OK(m)    <- m_OK(m_nest(m))
    # move the value from the nest to the outer position
    m_value(m) <- m_value(m_nest(m))
    m_nest(m)  <- m_delete_value(m_nest(m))
  }
  m
}

# FIXME: This is a bit of a hack. I have had some trouble getting the recursion
# depth set correctly at runtime, but it is fairly easy (see below) to set it
# after the run is complete. But this is not a good solution, since it leaves
# the monad in an incomplete state.
recursive_set_nest_depth <- function(m, i=1L){

  ms <- as.list(m, recurse_nests=FALSE)

  for(x in ms){
    if(is.na(m_nest_depth(x)))
      x$set_nest_depth(i)
  }

  for(x in ms){
    if(has_nest(x))
      recursive_set_nest_depth(m_nest(x), i+1L)
  }

  m

}
