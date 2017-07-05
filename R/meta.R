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
#' \code{unbranch} returns a list of monads, one for each branch
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
  m@stage@x <- m@x 
  m
}

#' @rdname rmonad_meta
#' @export 
esc <- function(m){
  if(m@OK){
    if(length(m@x) == 1){
      m@x[[1]]
    } else {
      m@x
    }
  } else {
    msg <- paste0('The call "', m@stage@code, '" failed: \n  ', m@stage@errors)
    stop(msg, call.=FALSE)
  }
}

#' @rdname rmonad_meta
#' @export 
forget <- function(m){
  m@history <- list()
  m
}

#' @rdname rmonad_meta
#' @export 
unbranch <- function(m){

  bs <- .unbranch_r(m)

  nfailed <- lapply(bs, m_OK) %>% unlist %>% `!` %>% sum
  n <- length(bs)

  errors <- if(nfailed > 0) {
    paste(nfailed, "of", n, "branches failed")
  } else {
    "" 
  }

  mu <- new("Rmonad",
    x = list(bs),
    OK = (nfailed == 0),
    stage = new("record", errors=errors)
  )

  mu
}
.unbranch_r <- function(m){
  bs <- append(forget(m), lapply(m@stage@branch, .unbranch_r) %>% unlist)
  bs <- append(bs, lapply(m@history, .unbranch_record) %>% unlist)
}
.unbranch_record <- function(r){
  lapply(r@branch, .unbranch_r) %>% unlist
}

#' @rdname rmonad_meta
#' @export
doc <- function(m, ...){
  m@stage@doc <- paste(list(...), collapse=" ")
  m
}
