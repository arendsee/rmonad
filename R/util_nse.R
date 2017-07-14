#' Extract docstring and meta data from an anonymous function
#'
#' Currently the docstring must come first and the metadata list must come
#' second. Both must preceed any other code in the function. These constraints
#' may be relaxed in the future.
#'
#' @param expr an expression with an optional docstring as the first statement
#' @return a list of three elements, the expression with the docstring and
#' metadata removed, the docstring itself, and the metadata.
#' @keywords internal
#' @export
#' @examples
#' expr <- substitute(
#'   {
#'     "this is the docstring"
#'     list(foo="this is meta data")
#'     5 * 32
#'   }
#' )
#' extract_metadata(expr)
extract_metadata <- function(expr){

  metadata <- list()
  docstring <- NULL

  # Determine if expr has the form
  #
  #  { "asdf" ; list(k=1) ; x ; y ... }
  #
  # And the ast:
  # \- ()
  #   \- `{
  #   \- "asdf"     # docstring
  #   \- ()
  #     \- `list
  #     \- 1
  #   \- `x
  #   \- `y
  #   \- ...
  if(
    is.call(expr) && # input must be call
    expr[[1]] == "{" # input is a code block
  ){
    # extract docstring
    if(
      length(expr) > 2 &&              # string is not the only element
      class(expr[[2]]) == "character"  # first element is a string
    ){
      docstring <- expr[[2]]
      expr <- expr[-2]
    }
    # extract metadata
    if(
      length(expr) > 2 &&                    # list is not the only element
      class(expr[[2]][[1]]) == "name" &&     # first element is a list
      as.character(expr[[2]][[1]]) == "list" # first element is a list
    ){
      metadata <- eval(expr[[2]])
      expr <- expr[-2]
    }
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
  #       \- ()            expr[[2]][[3]][[3]]  # metadata (if present)
  #         \- `list
  #         \- .... 
  #       \- y             expr[[2]][[3]] ...   # other statements
  #     \- <srcref>  # perhaps I could simplify some
  #                  # of my code if I used the srcref?
  #
  # I require
  #   1) expression matches this general template
  #   2) there is at least one expression following the docstring
  if(
    is.call(expr) &&
    expr[[1]] == "(" &&
    expr[[2]][[1]] == "function"
  ){
    if(
      length(expr[[2]][[3]]) > 2 &&
      class(expr[[2]][[3]][[2]]) == "character"
    ){
      # extract the docstring
      docstring <- expr[[2]][[3]][[2]]
      # remove the docstring from the expression
      expr[[2]][[3]] <- expr[[2]][[3]][-2]
      # reset the srcref
      expr[[2]][[4]] <- NULL
    }
    if(
      length(expr[[2]][[3]]) > 2 &&
      class(expr[[2]][[3]][[2]][[1]]) == "name" &&
      as.character(expr[[2]][[3]][[2]][[1]]) == "list"
    ){
      # extract the docstring
      metadata <- eval(expr[[2]][[3]][[2]])
      # remove the docstring from the expression
      expr[[2]][[3]] <- expr[[2]][[3]][-2]
      if(length(expr[[2]]) == 4){
        # reset the srcref
        expr[[2]][[4]] <- NULL
      }
    }
  }

  list(expr=expr, docstring=docstring, metadata=metadata)
}
