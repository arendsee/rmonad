#' Apply f to the contents of a monad and merge messages 
#'
#' This function uses non-standard evaluation to insert x into f as the first
#' positional argument. This allows specialization of f, but also prevents
#' higher-order voodoo from being performed.
#'
#' @export
#' @param x The input, may or may not be a monad report
#' @param f A function of the value contained in x
#' @param print_in logical - should the input be printed?
#' @param record_in logical - should the input be recorded?
#' @return A monad report
bind <- function(x, f, print_in=FALSE, record_in=FALSE){

  left_str = deparse(substitute(x))
  m <- as_rmonad(x, desc=left_str)
  if(print_in){ print(m@x) }

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
    if(record_in){ m@stage@x <- m@x }
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


#' Lift a value into the report monad if not in one already
#'
#' If you are not a haskeller, ignore the following message. \code{as_rmonad}
#' is almost a \code{return} function, except that if the input is already an
#' Rmonad, it is passed unchanged. \code{pass} is the true \code{return}
#' function.
#'
#' @export
#' @param x A value
#' @param ... extra state information for \code{pass}
#' @return Value wrapped in a report monad
as_rmonad <- function(x, ...){
  if (class(x) == "Rmonad") { x } else { pass(x, ...) }
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
