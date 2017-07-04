#' Take input and do nothing with it
#'
#' @param ... whatever
#' @export
toss <- function(...){ }

#' A dirty hack for documentation
#'
#' @param ... whatever
#' @export
note <- function(...){ }

#' Return the value of a possibly monadic input
#'
#' @param x rmonad or whatever
#' @export
esc <- function(x){
  m <- as_rmonad(x)
  m@x
}

#' Make NULL values an error
#'
#' @param x Input value
null_as_error <- function(x){
  stopifnot(! is.null(x))
}

#' Load a value into the report monad
#'
#' @export
#' @param x  The result of a successful computation
#' @param desc An optional description of the source
#' @return  The result wrapped in the report monad
pass <- function(x, desc=NULL) {
  if(class(x) == "Rmonad"){
    x
  } else {
    desc <- if(is.null(desc)) { deparse(substitute(x)) } else { desc }
    rec <- new("record", code=desc)
    new("Rmonad", x=list(x), stage=rec) 
  }
}
