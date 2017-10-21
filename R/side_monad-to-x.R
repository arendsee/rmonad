#' Make tabular summary of a pipeline
#'
#' @family from_Rmonad
#' @param m An Rmonad
#' @param code logical Should the code by included?
#' @export
mtabulate <- function(m, code=FALSE){
  # FIXME: I shouldn't need this once the accessor refactor is done
  optional_text <- function(x){
    if(is.null(x)) {
      rep(0, igraph::vcount(m@graph))
    } else {
      sapply(x, length)
    }
  }
  data.frame(
    code      = igraph::V(m@graph)$code %>% sapply(paste0, collapse="\n"),
    id        = igraph::V(m@graph) %>% as.numeric,
    OK        = igraph::V(m@graph)$OK,
    cached    = igraph::V(m@graph)$value %>% sapply(function(x) x@chk()),
    time      = igraph::V(m@graph)$time %>% { signif(.[1], 2) },
    space     = igraph::V(m@graph)$mem,
    # is_nested = has_nest(m),
    # nbranch   = length(m_branch(m)),
    nnotes    = igraph::V(m@graph)$notes    %>% optional_text,
    nwarnings = igraph::V(m@graph)$warnings %>% optional_text,
    error     = igraph::V(m@graph)$error    %>% optional_text,
    doc       = igraph::V(m@graph)$doc      %>% optional_text
  ) %>% {
    if(!code)
      .$code <- NULL
    .
  }
}

#' Tabulates all errors, warnings and notes
#' 
#' @family from_Rmonad
#' @param m An Rmonad
#' @export
missues <- function(m){
  optional_text <- function(x){
    if(is.null(x)) {
      character(0)
    } else {
      unlist(x)
    }
  }

  ids      <- igraph::V(m@graph) %>% as.numeric
  error    <- igraph::V(m@graph)$error    %>% optional_text
  warnings <- igraph::V(m@graph)$warnings %>% optional_text
  notes    <- igraph::V(m@graph)$notes    %>% optional_text
  data.frame(
    id = ids, # FIXME: this is borken
    type = c(
      rep("error", length(error)),
      rep("warning", length(warnings)),
      rep("note", length(notes))
    ),
    issue = c(error, warnings, notes)
  )
}

#' Convert a pipeline to Rmarkdown
#'
#' STUB
#'
#' @family from_Rmonad
#' @param m An Rmonad
#' @export
mreport <- function(m){
  stop("NOT IMPLEMENTED")
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
#' @family from_Rmonad
#' @param m An Rmonad
#' @param quiet If TRUE, print the exact messages that are raised, without
#'        extra context. 
#' @export 
esc <- function(m, quiet=FALSE){
  mtab <- mtabulate(m, code=TRUE)

  issues <- missues(m) %>%
    { merge(mtab, .)[, c("code", "type", "issue")] }

  if(quiet){
    fw <- .quiet_warning
    fn <- .quiet_note
    fe <- .quiet_error
  } else {
    fw <- .unquiet_warning
    fn <- .unquiet_note
    fe <- .unquiet_error
  }

  for(i in seq_len(nrow(issues))){
    # raise warnings, with contextual information
    if(issues[i, "type"] == "warning"){
      fw(issues[i, "code"], issues[i, "issue"])
    }
    # pass messages verbatim
    if(issues[i, "type"] == "note"){
      fn(issues[i, "code"], issues[i, "issue"])
    }
  }
  if(! m_OK(m)){
    fe(m_code(m), m_error(m))
  }

  m_value(m)
}

.quiet_warning <- function(code, msg) warning(msg, call.=FALSE)
.quiet_note    <- function(code, msg) message(msg)
.quiet_error   <- function(code, msg) stop(msg, call.=FALSE)

.unquiet_warning <- function(code, msg) {
  warning("in '", code, "': ", msg, call.=FALSE)
}
.unquiet_note <- function(code, msg) {
  message(msg)
}
.unquiet_error <- function(code, msg) {
  stop(paste0('in "', code, '":\n  ', msg), call.=FALSE)
}
