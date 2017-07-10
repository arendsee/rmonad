#' Make tabular summary of a monadic chain
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @export
mtabulate <- function(m){
  ms <- monad_to_list(m)
  do.call(rbind.data.frame, lapply(ms, .mtabulate)) %>%
    as.data.frame
}
.mtabulate <- function(m){
  list(
    id        = m_id(m),
    code      = paste(m_code(m), collapse="\n"),
    OK        = m_OK(m),
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
  ms <- monad_to_list(m)
  do.call(rbind.data.frame, lapply(ms, .missues)) %>%
    as.data.frame  # NOTE: this cast is required, since the above code
                   # silently mishandles the case or a zero-row data
                   # frame (it returns a list).
}
.missues <- function(m) {
  type <- c(
            rep.int("error",   length(m_error(m))    ),
            rep.int("warning", length(m_warnings(m)) ),
            rep.int("note",   length(m_notes(m))    )
           )
  issue <- as.character(c(m_error(m), m_warnings(m), m_notes(m)))
  id <- rep(m_id(m), length(type))
  list(id=id, type=type, issue=issue) 
}

#' Returns the value of a monad holds
#'
#' If the monad is in the passing state, return the wrapped value. Otherwise,
#' raise an appropriate error.
#'
#' Regardless of pass/fail status, \code{esc} raises all collected warnings and
#' prints all messages. Terminating a monadic sequence with \code{esc} should
#' obtain a result very close to running the same code outside the monad. The
#' main difference is that Rmonad appends the toplevel code that generated the
#' error.
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @export 
esc <- function(m){
  issues <- merge(mtabulate(m), missues(m))[, c("id", "code", "type", "issue")]
  for(i in seq_len(nrow(issues))){
    # raise warnings, with contextual information
    if(issues[i, "type"] == "warning"){
      warning("in '", issues[i, "code"], "': ", issues[i, "issue"], call.=FALSE)
    }
    # pass messages verbatim
    if(issues[i, "type"] == "note"){
      message(issues[i, "issue"])
    }
  }
  if(! m_OK(m)){
    # if the final state is failing, raise error with contextual info
    msg <- paste0('in "', m_code(m), '":\n  ', m_error(m))
    stop(msg, call.=FALSE)
  }
  m_value(m)
}



#' Functions for extracting information from Rmonad history
#'
#' Each of these functions returns a list of the values for the eponymous slot
#' (e.g. uncode returns the code run for each past stage).
#'
#' \code{unbranch} is recursive, returning a monad for each branch. The other
#' functions are currently not recursive (do not look into branches). I will
#' change this eventually.
#'
#' @param m An Rmonad
#' @name rmonad_unwrap
NULL

#' @rdname rmonad_unwrap
#' @export
unstore <- function(m) {
  lapply(monad_to_list(m), m_value)
}

#' @rdname rmonad_unwrap
#' @export
uncode <- function(m) {
  lapply(monad_to_list(m), m_code)
}

#' @rdname rmonad_unwrap
#' @export
unerror <- function(m) {
  lapply(monad_to_list(m), m_error)
}

#' @rdname rmonad_unwrap
#' @export
unwarnings <- function(m) {
  lapply(monad_to_list(m), m_warnings)
}

#' @rdname rmonad_unwrap
#' @export
unnotes <- function(m) {
  lapply(monad_to_list(m), m_notes)
}

#' @rdname rmonad_unwrap
#' @export
undoc <- function(m) {
  lapply(monad_to_list(m), m_doc)
}

#' @rdname rmonad_unwrap
#' @export
untime <- function(m) {
  lapply(monad_to_list(m), m_time)
}

#' @rdname rmonad_unwrap
#' @export 
unbranch <- function(m){

  ms <- monad_to_list(m)

  bs <- lapply(ms, m_branch) %>% unlist

  bs <- unique(list(m) %++% bs)

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
