#' Take input and do nothing with it
#'
#' @param ... whatever
#' @export
toss <- function(...){ }

#' Return the value of a possibly monadic input
#'
#' @param x rmonad or whatever
#' @export
esc <- function(x){
  m <- as_rmonad(x)
  m@x
}

#' Load a value into the report monad
#'
#' @export
#' @param x  The result of a successful computation
#' @param desc An optional description of the source
#' @return  The result wrapped in the report monad
#' @examples
#' foo <- function(x) {
#'   if(x <= 0){
#'     fail(x, "x <= 0, cannot log")
#'   } else {
#'     pass(log(x))
#'   }
#' }
#' foo(-1)
#' foo(2)
pass <- function(x, desc=NULL) {
  if(class(x) == "Rmonad"){
    x
  } else {
    desc <- if(is.null(desc)) { deparse(substitute(x)) } else { desc }
    rec <- new("record", code=desc)
    new("Rmonad", x=list(x), stage=rec) 
  }
}

#' Load a failure message into the monad
#'
#' @export
#' @param x A value (which is be ignored)
#' @param s An error message
#' @return A failing monad report
fail <- function(x, s) {
  if(class(x) == "Rmonad"){
    x@stage@errors <- append(x@stage@errors, s)
    x@OK <- FALSE
  } else {
    rec <- new("record", code=deparse(substitute(x)), errors=list(s))
    x <- new("Rmonad", stage=rec, OK=FALSE)
  }
  x
}

#' Append a warning message onto the monad
#'
#' @export
#' @param m A report monad
#' @param s A string describing a warning
#' @param force logical, should we add the note even to a failed monad?
#' @return A report monad with a new warning appended
warn <- function(m, s, force=FALSE) {
  m <- as_rmonad(m, desc=deparse(substitute(m)))
  if(m@OK || force){
    m@stage@warnings <- append(m@stage@warnings, s)
  }
  m
}

#' Append a note message onto the monad
#'
#' @export
#' @param m A report monad
#' @param s A string describing a note
#' @param force logical, should we add the note even to a failed monad?
#' @return A report monad with a new note appended
note <- function(m, s, force=FALSE) {
  m <- as_rmonad(m, desc=deparse(substitute(m)))
  if(m@OK || force){
    m@stage@notes <- append(m@stage@notes, s)
  }
  m
}
