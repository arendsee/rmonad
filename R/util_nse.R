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
    es[[1]] == "{"                && # the input is a code block
    class(es[[2]]) == "character" && # the first element is a string
    length(es) > 2                   # the string is not the only element

  if(has_docstring){
    docstring <- es[[2]]
    expr <- as.call(es[-2])
  } else {
    docstring <- ""
  }
  list(expr=expr, docstring=docstring)
}
