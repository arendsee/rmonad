#' Functions on monads
#'
#' These functions act on Rmonad objects, rather than specifically the values
#' they contain.
#'
#' \code{store} stores the monads value in the monads state, so that it is
#' passed down the chain.
#'
#' \code{esc} returns the value of a monad or, if the monad is in the failing
#' state, raises on appropriate error.
#'
#' \code{forget} wipes a monads history.
#'
#' \code{doc} adds a documentation section to the monad.
#'
#' @param m An Rmonad
#' @param ... additional arguments
#' @name rmonad_meta

NULL


#' @rdname rmonad_meta
#' @export 
store <- function(m){
  # NOTE: this is the public API to an internal function
  .store(m)
}

#' @rdname rmonad_meta
#' @export 
esc <- function(m){
  if(m_OK(m)){
    m_value(m)
  } else {
    msg <- paste0('The call "', m_code(m), '" failed: \n  ', m_error(m))
    stop(msg, call.=FALSE)
  }
}

#' @rdname rmonad_meta
#' @export 
forget <- function(m){
  m_history(m) <- list()
  m
}

#' @rdname rmonad_meta
#' @export
doc <- function(m, ...){
  m_doc(m) <- paste(list(...), collapse=" ")
  m
}
