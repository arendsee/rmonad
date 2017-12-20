#' Extract docstring and meta data from an anonymous function
#'
#' Currently the docstring must come first and the metadata list must come
#' second. Both must precede any other code in the function. These constraints
#' may be relaxed in the future.
#'
#' @param expr an expression with an optional docstring as the first statement
#' @param env Environment in which to search for functions
#' @param skip_name Do not attempt to find function matching expressions of
#' type 'name'. If FALSE, then a function will by searched for with name
#' \code{expr} if \code{expr} is a name. This leads to \code{expr} being
#' evaluated, which raises errors outside the purview of Rmonad. For example,
#' \code{extract_metadata(stop("dying"))}.
#' @return a list of three elements, the expression with the docstring and
#' metadata removed, the docstring itself, and the metadata.
#' @keywords internal
#' @export
#' @examples
#' ## extract metadata from a block 
#' expr <- substitute(
#'   {
#'     "this is the docstring"
#'     list(foo="this is meta data")
#'     5 * 32
#'   }
#' )
#' extract_metadata(expr)
#'
#' foo <- function(x,y){
#'   "docstring"
#'   list(meta="data")
#'   x + y
#' }
#' ## extract metadata from a function name
#' extract_metadata(substitute(foo), skip_name=FALSE)
#'
#' ## extract from a partially applied function
#' extract_metadata(substitute(foo(y=2)))
extract_metadata <- function(expr, env=parent.frame(), skip_name=TRUE){

  metadata <- list()
  docstring <- .default_doc()

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
      metadata <- eval(expr[[2]], envir=env)
      expr <- expr[-2]
    }
  }

  # I want to match an expression like:
  #
  #  (function(x){"asdf" ; y})
  #
  # The body of the function is the same as what was processed above. So I can
  # just recurse on the body.
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
  else if(
    is.call(expr) &&
    expr[[1]] == "(" &&
    expr[[2]][[1]] == "function"
  ){
    bod <- extract_metadata(expr[[2]][[3]], env=env)
    docstring <- bod$docstring    
    metadata <- bod$metadata
    expr[[2]][[3]] <- bod$expr
  }

  else if(
    is.call(expr) &&
    length(expr[[1]]) == 1 && # i.e. not in a namespace (foo::bar)
    methods::existsFunction(as.character(expr[[1]]), where=env)
  ) {
    meta <- extract_metadata(expr[[1]], env=env, skip_name=FALSE)
    docstring <- meta$docstring
    metadata <- meta$metadata
  }

  # Handle named functions
  else if (
    ! skip_name &&
    is.name(expr) &&
    methods::existsFunction(as.character(expr), where=env)
  ){
    x <- get( as.character(expr), envir=env )
    if(!is.null(body(x))){
      bod <- extract_metadata(body(x), env=env)
      docstring <- bod$docstring
      metadata <- bod$metadata
    }
  }


  list(expr=expr, docstring=docstring, metadata=metadata)
}
