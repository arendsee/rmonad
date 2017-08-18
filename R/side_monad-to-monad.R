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

#' Remove a level of nesting in an Rmonad
#'
#' @family monad-to-monad
#' @param m An Rmonad
#' @export
unnest <- function(m){
  if(is_rmonad(m) && .has_value(m) && is_rmonad(m_value(m))){

    m_nest(m)  <- .set_recursion_depth(m_value(m), m_nest_depth(m)+1L)
    m_OK(m)    <- m_OK(m_nest(m))
    # move the value from the nest to the outer position
    m_value(m) <- m_value(m_nest(m))
    m_nest(m)  <- m_delete_value(m_nest(m))
  }
  m
}

.set_recursion_depth <- function(m, i){
  m_parents(m) <- lapply(m_parents(m), .set_recursion_depth, i) 
  if(m_nest_depth(m) == i - 1){
    m_nest_depth(m) <- i
  }
  if(.has_nest(m)){
    m_nest(m) <- .set_recursion_depth(m_nest(m), i+1L)
  }
  m
}
