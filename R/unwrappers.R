#' Functions for extracting information from Rmonad history
#'
#' Each of these functions returns a list of the values for the eponymous slot
#' (e.g. uncode returns the code run for each past stage).
#'
#' \code{unbranch} is recursive, returning a monad for each branch. The other
#' functions are currently not recursive (do not look into branches). I will
#' change this eventualy.
#'
#' @param m An Rmonad
#' @name rmonad_unwrap
NULL

#' @rdname rmonad_unwrap
#' @export
unstore <- function(m) {
  lapply(m_history(m) %+% m, m_value)
}

#' @rdname rmonad_unwrap
#' @export
uncode <- function(m) {
  lapply(m_history(m) %+% m, m_code)
}

#' @rdname rmonad_unwrap
#' @export
unerror <- function(m) {
  lapply(m_history(m) %+% m, m_error)
}

#' @rdname rmonad_unwrap
#' @export
unwarnings <- function(m) {
  lapply(m_history(m) %+% m, m_warnings)
}

#' @rdname rmonad_unwrap
#' @export
unnotes <- function(m) {
  lapply(m_history(m) %+% m, m_notes)
}

#' @rdname rmonad_unwrap
#' @export
undoc <- function(m) {
  lapply(m_history(m) %+% m, m_doc)
}

#' @rdname rmonad_unwrap
#' @export 
unbranch <- function(m){

  bs <- .unbranch_r(m)

  nfailed <- lapply(bs, m_OK) %>% unlist %>% `!` %>% sum
  n <- length(bs)

  error <- if(nfailed > 0) {
    paste(nfailed, "of", n, "branches failed")
  } else {
    "" 
  }

  mu <- new("Rmonad",
    x = list(bs),
    OK = (nfailed == 0),
    stage = new("record", error=error)
  )

  mu
}
.unbranch_r <- function(m){
  bs <- append(forget(m), lapply(m_branch(m), .unbranch_r) %>% unlist)
  bs <- append(bs, lapply(m_history(m), .unbranch_record) %>% unlist)
}
.unbranch_record <- function(r){
  lapply(m_branch(r), .unbranch_r) %>% unlist
}
