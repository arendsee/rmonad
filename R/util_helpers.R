#' Take input and do nothing with it
#'
#' @param ... whatever
#' @export
toss <- function(...){ }

#' The identity function
#'
#' @param x anything
#' @param ... parameters that will be ignored
#' @export
#' @examples
#' # pull a value out of failure
#' 1:10 %$>% colSums %|>% ident
#' # here it just does nothing
#' cars %$>% colSums %|>% ident
ident <- function(x, ...){ x }

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
