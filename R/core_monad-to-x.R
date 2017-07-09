#' Returns the value of a monad holds
#'
#' If the monad is in the passing state, return the wrapped value. Otherwise,
#' raise an appropriate error.
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @export 
esc <- function(m){
  if(m_OK(m)){
    m_value(m)
  } else {
    msg <- paste0('The call "', m_code(m), '" failed: \n  ', m_error(m))
    stop(msg, call.=FALSE)
  }
}

#' Make tabular summary of a monadic chain
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @export
mtabulate <- function(m){
  do.call(rbind.data.frame, lapply(m_history(m) %+% m, .mtabulate)) %>%
    as.data.frame
}
.mtabulate <- function(m){
  list(
    id        = m_id(m),
    code      = m_code(m),
    cached    = !is.null(m_value(m)),
    time      = signif(m_time(m)[1], 2),
    space     = m_mem(m),
    nbranch   = length(m_branch(m)),
    nnotes    = length(m_notes(m)),
    nwarnings = length(m_warnings(m)),
    error     = length(m_error(m)),
    doc       = length(m_doc(m))
  )
}

#' Tabulates all errors, warnings and notes
#' 
#' @family monad-to-x
#' @param m An Rmonad
#' @export
missues <- function(m){
  do.call(rbind.data.frame, lapply(m_history(m) %+% m, .missues)) %>%
    as.data.frame  # NOTE: this cast is required, since the above code
                   # silently mishandles the case or a zero-row data
                   # frame (it returns a list).
}
.missues <- function(m) {
  type <- c(
            rep.int("error",   length(m_error(m))    ),
            rep.int("warning", length(m_warnings(m)) ),
            rep.int("notes",   length(m_notes(m))    )
           )
  issue <- as.character(c(m_error(m), m_warnings(m), m_notes(m)))
  id <- rep(m_id(m), length(type))
  list(id=id, type=type, issue=issue) 
}




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
untime <- function(m) {
  lapply(m_history(m) %+% m, m_time)
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
    NULL
  }

  mu <- new_rmonad()
  m_value(mu) <- bs
  m_OK(mu)    <- nfailed == 0
  m_error(mu) <- error

  mu
}
.unbranch_r <- function(m){
  bs <- append(forget(m), lapply(m_branch(m), .unbranch_r) %>% unlist)
  bs <- append(bs, lapply(m_history(m), .unbranch_record) %>% unlist)
}
.unbranch_record <- function(r){
  lapply(m_branch(r), .unbranch_r) %>% unlist
}
