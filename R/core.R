#' A record of past events 
#'
#' @slot code     A string showing the function that created record's report 
#' @slot errors   List of errors accumulated so far 
#' @slot warnings List of warnings accumulated so far
#' @slot notes    List of notes accumulated so far
record <- setClass(
  "record",
  representation(
    code     = "character",
    errors   = "list",
    warnings = "list",
    notes    = "list"
  ),
  prototype(
    code     = "",
    errors   = list(),
    warnings = list(),
    notes    = list() 
  )
)

print.record <- function(x, ...) {
  if(length(x@code) > 0){
    cat(sprintf("R> %s\n", x@code)) 
  }
  if(length(x@errors) != 0){
    cat("Error: ")
    cat(paste(unlist(x@errors), collapse="\nError: "))
    cat("\n")
  }
  if(length(x@warnings) != 0){
    cat("Warning: ")
    cat(paste(unlist(x@warnings), collapse="\nWarning: "))
    cat("\n")
  }
  if(length(x@notes) != 0){
    cat("Note: ")
    cat(paste(unlist(x@notes), collapse="\nNote: "))
    cat("\n")
  }
}

#' The eponymous monad type
#'
#' @slot x       List of values produced if the parent computation succeeded
#' @slot stage   The active record 
#' @slot history A list of past records
#' @slot OK      TRUE if the report is currently passing
Rmonad <- setClass(
  "Rmonad",
  representation(
    x       = "list",
    stage   = "record",
    history = "list",
    OK      = "logical"
  ),
  prototype(
    x       = list(),
    stage   = new("record"),
    history = list(),
    OK      = TRUE
  )
)

print.rmonad <- function(x, ...){
  print(x@stage)

  if(length(x@x) == 1){
    cat("x: ")
    print(x@x[[1]])
  } else if(length(x@x > 1)) {
    print(x@x)
  }

  if(length(x@history) > 0){
    cat("\n ----------------- \n\n")
    f <- lapply(x@history, function(x) {print(x); cat("\n")})
  }
}

#' Merge list of reports into one
#'
#' @export
#' @param ms  List of report
#' @return A combined report
combine <- function(ms){
  ms <- lapply(ms, as_rmonad)
  rec <- new("record")
  out <- new(
     "Rmonad",
     x        = list(),
     stage    = rec,
     history  = Reduce( append, lapply(ms, function(m) append(m@stage, m@history)), list() ),
     OK       = FALSE
  )
  if(all(sapply(ms, function(m) m@OK))){
    out@x  <- Reduce( append, lapply(ms, function(m) m@x), list() )
    out@OK <- TRUE
  }
  out 
}


#' Lift a value into the report monad if not in one already
#'
#' @export
#' @param x A value
#' @param ... extra state information for \code{pass}
#' @return Value wrapped in a report monad
as_rmonad <- function(x, ...){
  if (class(x) == "Rmonad") { x } else { pass(x, ...) }
}

#' Apply f to the contents of a monad and merge messages 
#'
#' This function uses non-standard evaluation to insert x into f as the first
#' positional argument. This allows specialization of f, but also prevents
#' higher-order voodoo from being performed.
#'
#' @export
#' @param x The input, may or may not be a monad report
#' @param f A function of the value contained in x
#' @return A monad report
#' @examples
#' bind(5, runif(min=10, max=20))
bind <- function(x, f){

  left_str = deparse(substitute(x))

  m <- as_rmonad(x, desc=left_str)

  if(m@OK)
  {
    # insert x as first positional in f
    fs    <- substitute(f)
    fl    <- as.list(fs)
    func  <- as.character(fl[[1]])
    fargs <- append(m@x, fl[-1])
    envir <- parent.frame()
    y     <- as_rmonad( do.call(func, fargs, envir=envir) )
    # merge notes and warnings, replace value
    y@stage@code <- deparse(fs)
    y@history    <- append(m@stage, m@history)
  }
  else
  {
    # propagate error
    y <- m
  }
  y
}

#' Infix version of \code{bind}
#'
#' @export
#' @param l left-hand side
#' @param r right-hand side
#' @return result of bind(l, r)
`%>>=%` <- function(l, r) {
    envir <- parent.frame()
    eval(as.call(list(bind, substitute(l), substitute(r))), envir=envir)
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
