#' Make tabular summary of a monadic chain
#'
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
