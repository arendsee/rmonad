#' The eponymous monad type
#'
#' @slot x List of values produced if the parent computation succeeded
#' @slot errors List of errors accumulated so far 
#' @slot warnings List of warnings accumulated so far
#' @slot notes List of notes accumulated so far
#' @slot OK TRUE if the report is currently passing
monadR <- setClass(
  "monadR",
  representation(
    x        = "list",
    errors   = "list",
    warnings = "list",
    notes    = "list",
    OK       = "logical"
  ),
  prototype(
    x        = list(),
    errors   = list(),
    warnings = list(),
    notes    = list(),
    OK       = TRUE
  )
)

#' The monoid zero element for monad report
#'
#' The report monad is also a monoid. Which means there is 1) an associative
#' operator that can merge reports (\code{mcombine}) and 2) a zero element.
#' \code{mzero} represents the zero element.
#'
mzero <- function() { methods::new("monadR") }

#' Combine two reports into one
#'
#' This is the associative binary operator that, together with the zero element
#' \code{mzero}, define the report monad as a monoid.
#'
#' Since each slot in the monadR class is a list, two reports can be
#' combined simple by appending each list in m2 to the corresponding list in
#' m1.
#'
#' @export
#' @param m1  Report monad
#' @param m2  Report monad
#' @return A report monad with combines m1 and m2 
mcombine <- function(m1, m2){
  m1 <- as.monadR(m1)
  m2 <- as.monadR(m2)
  m <- methods::new(
     "monadR",
     x        = list(),
     errors   = append(m1@errors,   m2@errors),
     notes    = append(m1@notes,    m2@notes),
     warnings = append(m1@warnings, m2@warnings),
     OK       = FALSE
  )
  if(m1@OK && m2@OK){
    m@x  <- append(m1@x, m2@x)
    m@OK <- TRUE
  }
  m
}

#' Load a value into the report monad
#'
#' @export
#' @param x  The result of a successful computation
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
pass <- function(x) {
  if(class(x) == "monadR"){
    x
  } else {
    methods::new("monadR", x=list(x)) 
  }
}

#' Load a failure message into the monad
#'
#' @export
#' @param x A value (which is be ignored)
#' @param s An error message
#' @return A failing monad report
fail <- function(x, s) {
  if(class(x) == "monadR"){
    x@errors <- append(x@errors, s)
    x@OK <- FALSE
  } else {
    x <- methods::new("monadR", errors=list(s), OK=FALSE)
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
  m <- as.monadR(m)
  if(m@OK || force){
    m@warnings <- append(m@warnings, s)
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
  m <- as.monadR(m)
  if(m@OK || force){
    m@notes <- append(m@notes, s)
  }
  m
}

#' Lift a value into the report monad if not in one already
#'
#' @export
#' @param x A value
#' @return Value wrapped in a report monad
as.monadR <- function(x){
  if (class(x) == "monadR") { x } else { pass(x) }
}

#' Apply a function to several arguments
#'
#' This function is a multivariate wrapper for bind. All elements in the list
#' of input arguments, \code{xs}, are first cast into the \code{monadR} class
#' (if not in it already). Then these are merged into one report monad. This
#' report monad is then sent to \code{bind}. You can think of it as merging
#' many arguments into a single tuple of arguments.
#'
#' @export
#' @param xs A list of inputs to f, these may or may not be in report monads 
#' @param f some function of xs
#' @return a result in a report monad
bindMerge <- function(xs, f){
  # load inputs in error container if the are not already in one
  ms <- lapply(as.monadR, xs)

  # combine all report monads into one
  m <- Reduce(mcombine, ms, mzero()) 

  # send to the binary bind operator
  bind(m, f)
}

#' Apply f to the contents of a monad and merge messages 
#'
#' @export
#' @param x The input, may or may not be a monad report
#' @param f A function of the value contained in x
#' @return A monad report
bind <- function(x, f){
  m <- as.monadR(x)
  if(m@OK)
  {
    # merge notes and warnings, replace value
    y <- as.monadR( do.call(f, m@x) )
    y@notes    <- append(m@notes,    y@notes)
    y@warnings <- append(m@warnings, y@warnings)
  }
  else
  {
    # propagate error
    y <- m
  }
  y
}
