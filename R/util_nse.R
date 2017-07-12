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

  has_docstring <-
    is.call(expr)                   && # input must be call
    expr[[1]] == "{"                && # input is a code block
    class(expr[[2]]) == "character" && # first element is a string
    length(expr) > 2                   # string is not the only element

  if(has_docstring){
    docstring <- expr[[2]]
    expr <- as.call(expr[-2])
  } else {
    docstring <- NULL
  }
  list(expr=expr, docstring=docstring)
}
