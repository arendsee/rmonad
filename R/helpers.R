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

#' The identity function
#'
#' @param x anything
#' @export
#' @examples
#' # pull a value out of failure
#' 1:10 %$>% colSums %?>% id
#' # here it just does nothing
#' cars %$>% colSums %?>% id
id <- function(x){ x }

#' Ignore the first input, return the second
#'
#' This function can be used to change the value in the lhs of a monadic
#' sequence
#'
#' @param x ignored value
#' @param r replacing value
#' @export
const <- function(x, r){ r }

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
