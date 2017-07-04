#' Apply f to the contents of a monad and merge messages 
#'
#' This function should not usually be used directly. Rather you should use the
#' infix operators. They all wrap this function.
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
#' @param branch Store the output in the branch slot
#' @param discard_out Ignore the result of the rhs function, passon the left
#' @param discard_in Ignore the input
#' @param handle  Operate on a failed state
#' @return A monad report
bind <- function(
  x,
  f,
  print_in    = FALSE,
  record_in   = FALSE,
  branch      = FALSE,
  discard_out = FALSE,
  discard_in  = FALSE,
  handle      = FALSE
){

  left_str = deparse(substitute(x))
  m <- as_rmonad(x, desc=left_str)
  if(print_in){ print(m@x) }

  if(handle && m@OK){ return(m) } 

  if(m@OK || handle)
  {

    # insert x as first positional in f
    fs    <- substitute(f)
    fl    <- as.list(fs)
    func  <- as.character(fl[[1]])
    envir <- parent.frame()
    fargs <- append(m@x, fl[-1])
    y     <- mrun( do.call(func, fargs, envir=envir) )

    y@stage@code <- deparse(fs)

    # merge notes and warnings, replace value
    if(record_in){ m@stage@x <- m@x }

    if(branch){
      m@stage@branch <- append(m@stage@branch, y)
      o <- m
    } else {
      if(!y@OK || discard_out){
        # On failure, propagate the final passing value, this allows
        # for either degugging or passage to alternative handlers.
        y@x <- m@x
      }
      y@history <- append(m@history, m@stage)
      o <- y
    }
  }
  else
  {
    # propagate error
    o <- m
  }
  o
}


#' Run an expression, capture EWM, return Rmonad
#'
#' @param expr An expression
#' @param desc A name to assign to the code slot
#' @return Rmonad object 
#' @export
mrun <- function(expr, desc=NULL){

  value <- NULL 
  warns <- list()
  fails <- list()
  isOK  <- TRUE

  notes <- capture.output(
    {
      value <- withCallingHandlers(
        tryCatch(
          expr,
          error = function(e) {
            fails <<- list(e$message);
            isOK <<- FALSE
          }
        ),
        warning = function(w){
          warns <<- append(warns, w$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    type="message"
  )

  value <- if(isOK) { list(value) } else { list() }

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  new("Rmonad",
    x = value,
    stage = new("record",  
      x        = list(),
      code     = code,
      errors   = fails,
      warnings = as.list(warns),
      notes    = as.list(notes)
    ),
    history = list(),
    OK = isOK
  )

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
#' @param keep_history Merge the histories of all monads
#' @return A combined report
combine <- function(ms, keep_history=TRUE){
  ms <- lapply(ms, as_rmonad)
  rec <- new("record")
  history <- if(keep_history) {
    Reduce( append, lapply(ms, function(m) append(m@history, m@stage)), list() )
  } else {
    list()
  }
  out <- new(
     "Rmonad",
     x        = list(),
     stage    = rec,
     history  = history,
     OK       = FALSE
  )
  if(all(sapply(ms, function(m) m@OK))){
    out@x  <- Reduce( append, lapply(ms, function(m) m@x), list() )
    out@OK <- TRUE
  }
  out 
}
