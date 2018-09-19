#' Apply an rmonad pipeline function to each element in a rmonad bound list 
#'
#' @param m Rmonad object wrapping a vector
#' @param FUN function of an element from the vector stored in \code{m} that
#' returns an Rmonad object.
#' @param looper function that applies each element in the input vector to
#' \code{FUN}. The default it \code{lapply}.
#' @param ... Additional arguments sent to \code{FUN}
#' @return Rmonad object wrapping a vector of the values wrapped by the outputs
#' of \code{FUN}
#' @export
#' @examples
#' foo <- function(x) { x %>>% sqrt }
#' c(256, 6561) %v>% sqrt %>% loop(foo) %>>% lapply(sqrt)
loop <- function(m, FUN, looper=lapply, ...){
  # m [a] -> (a -> m b) -> ([c] -> [d]) -> m [b]
  .m_check(m)

  if(!get_OK(m, m@head)){
    return(m)
  }

  if(!has_value(m, m@head)){
    # This error is raised OUTSIDE of the monad, since a violations will
    # usually be due to a coding error on programmers part.
    # TODO: could be more helpful, print out the incoming function, or
    # something
    stop("Cannot loop over this, no values found.")
  }

  indexed_looper <- (function(){
    i=0
    function(x, ...){
      i <<- i + 1
      .set_nest_salt(get_key(m, m@head)[[1]], serialize(i, NULL))
      FUN(x, ...)
    }
  })()

  xs <- get_value(m, m@head)[[1]]
  ns <- looper(xs, indexed_looper, ...)
  if(! all(sapply(ns, is_rmonad))){
    stop("FUN must return a vector or Rmonad objects")
  }

  m2 <- combine(ns)
  .inherit(
    child=m2,
    parent=m,
    type          = "depend",
    inherit_value = FALSE,
    inherit_OK    = FALSE,
    force_keep    = FALSE
  )
}
