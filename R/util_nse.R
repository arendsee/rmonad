#' Extract a docstring from an anonymous function
#'
#' @param expr an expression with an optional docstring as the first statement
#' @return a list of two elements, the expression with the docstring removed,
#'         and the docstring itself
#' @keywords internal
#' @export
#' @examples
#' expr <- substitute(
#'   {
#'     "this is the docstring"
#'     5 * 32
#'   }
#' )
#' extract_docstring(expr)
extract_docstring <- function(expr){

  # Determine if expr has the form
  #
  #  { "asdf" ; x ; y ... }
  #
  # And the ast:
  # \- ()
  #   \- `{
  #   \- "asdf"
  #   \- `x
  #   \- `y
  #   \- ...
  if(
    is.call(expr)                   && # input must be call
    expr[[1]] == "{"                && # input is a code block
    length(expr) > 2                && # string is not the only element
    class(expr[[2]]) == "character"    # first element is a string
  ){
    docstring <- expr[[2]]
    expr <- expr[-2]
  } else {
    docstring <- NULL
  }

  # The code below isn't pretty, but here is what it is doing:
  #
  # I want to match an expression like:
  #
  #  (function(x){"asdf" ; y})
  #
  # This has the ast (with the help of 'pryr')
  # \- ()
  #   \- `(                expr[[1]]
  #   \- ()                expr[[2]]
  #     \- `function       expr[[2]][[1]]
  #     \- []              expr[[2]][[2]]       # arguments
  #        \ x = `MISSING
  #     \- ()              expr[[2]][[3]]       # body
  #       \- `{            expr[[2]][[3]][[1]]
  #       \- "asdf"        expr[[2]][[3]][[2]]  # docstring (if present)
  #       \- y             expr[[2]][[3]] ...   # other statements
  #     \- <srcref>  # perhaps I could simplify some
  #                  # of my code if I used the srcref?
  #
  # I require
  #   1) expression matches this general template
  #   2) there is at least one expression following the docstring
  if(
    is.call(expr)                             &&
    expr[[1]] == "("                          &&
    expr[[2]][[1]] == "function"              &&
    length(expr[[2]][[3]]) > 2                &&
    class(expr[[2]][[3]][[2]]) == "character"
  ){

    # extract the docstring
    docstring <- expr[[2]][[3]][[2]]

    # remove the docstring from the expression
    expr[[2]][[3]] <- expr[[2]][[3]][-2]

    # reset the srcref
    expr[[2]][[4]] <- NULL

  }

  list(expr=expr, docstring=docstring)
}
