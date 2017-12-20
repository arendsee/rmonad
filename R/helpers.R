# TODO: settle on a set of helper functions for export

#' Take input and do nothing with it
#'
#' @family helper_functions
#' @param ... whatever
toss <- function(...){ }

#' Given two arguments, return the first
#'
#' @family help_functions
#' @param x anything
#' @param y anything
first <- function(x, y) x

#' Given two arguments, return the second
#'
#' @family help_functions
#' @param x anything
#' @param y anything
second <- function(x, y) y

#' Do nothing
#'
#' @family help_functions
#' @param ... anything
#' @return nothing
nothing <- function(...) { invisible() }

#' Return false for all input
#'
#' @family helper_functions
#' @param ... whatever
false <- function(...) { FALSE }

#' Return true for all input
#'
#' @family helper_functions
#' @param ... whatever
true  <- function(...) { TRUE  }

#' Ignore the first input, return the second
#'
#' This function can be used to change the value in the lhs of a monadic
#' sequence
#'
#' @family helper_functions
#' @param x ignored value
#' @param r replacing value
const <- function(x, r){ r }

#' Make NULL values an error
#'
#' Currently not exported.
#'
#' @family helper_functions
#' @param x Input value
null_as_error <- function(x){
  stopifnot(! is.null(x))
}

#' Make NULL values an error
#'
#' @family helper_functions
#' @param x Input value
false_as_error <- function(x){
  stopifnot(! is.null(x))
}
