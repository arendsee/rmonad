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
  es <- as.list(expr) 

  has_docstring <-
    is.call(expr)                 && # input must be call
    es[[1]] == "{"                && # input is a code block
    class(es[[2]]) == "character" && # first element is a string
    length(es) > 2                   # string is not the only element

  if(has_docstring){
    docstring <- es[[2]]
    expr <- as.call(es[-2])
  } else {
    docstring <- NULL
  }
  list(expr=expr, docstring=docstring)
}
