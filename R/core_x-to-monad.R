#' Conversions to monads
#'
#' These functions convert possibly non-monadic inputs into monads.
#'
#' For each of these functions, failure of any part causes failure of the
#' whole. Any non-monadic inputs will be converted to monads. Any exceptions
#' raised in the inputs will be caught.
#'
#' \code{as_monad} evaluate a single expression into an Rmonad. If the value is
#' already an Rmonad, the existing value is returned.
#'
#' \code{funnel} evaluates multiple arguments into one Rmonad. It can be used
#' within pipelines to create multi-input nodes (works well with \code{\%*>\%}).
#'
#' \code{combine} takes a list and joins the elements into one Rmonad. If
#' passed a list of non-monadic values, this will create a new monad that wraps
#' a list, but it records each element of the list as a parent monad. It only
#' really makes sense to use \code{combine} on lists of monads.
#'
#' @param expr An expression
#' @param xs  A list of elements to join into a monad
#' @param doc A docstring to associate with the monad
#' @param desc A description of the monad (usually the producing code)
#' @param keep_history Merge the histories of all monads
#' @param ... multiple expressions
#' @name x_to_monad
#' @examples
#' as_monad(stop(1))
#' as_monad(1:10)
#' as_monad(5 %>>% sqrt)
#'
#' ## merge failing inputs 
#' funnel( 1:10, stop(1), sqrt(-3:3) )
#'
#' ## join pipelines
#' b2 <- letters[1:10] %>>% sqrt
#' b3 <- -3:6 %>>% log
#' 1:10 %>% funnel(b2,b3) %>>%
#'   {data.frame(b1=.[[1]], b2=.[[2]], b3=.[[3]])}
#'
#' z <- list(
#'   x = rnorm(10) %>>% sqrt,
#'   y = 1 %>>% colSums
#' )
#' combine(z)
NULL


#' @rdname x_to_monad
#' @export
as_monad <- function(expr, desc=NULL, doc=NULL){
# as_monad :: a -> m a

  value <- NULL 
  warns <- NULL
  fails <- NULL
  isOK  <- TRUE

  env <- parent.frame()
  ed <- extract_metadata(substitute(expr))
  expr <- ed$expr
  doc <- ed$docstring

  notes <- capture.output(
    {
      value <- withCallingHandlers(
        tryCatch(
          eval(expr, envir=env),
          error = function(e) {
            fails <<- e$message;
            isOK <<- FALSE
          }
        ),
        warning = function(w){
          warns <<- append(warns, w$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    type="message"
  )

  if(length(value) == 1 && class(value) == "Rmonad") { return(value) }

  if(!isOK) {
    value = NULL
  }

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  m <- new_monad()

  # These accessors do the right thing (don't mess with them)
  m_value(m)    <- value
  m_code(m)     <- code
  m_error(m)    <- fails
  m_warnings(m) <- warns
  m_notes(m)    <- notes
  m_OK(m)       <- isOK
  m_doc(m)      <- doc

  m

}


#' @rdname x_to_monad
#' @export
funnel <- function(..., keep_history=TRUE){
# funnel :: [Rexpr] -> m [*]
  desc <- deparse(match.call())

  e <- parent.frame()

  # don't linearize with magrittr, thar lie dragons
  combine(
    lapply(
      as.list(substitute(alist(...)))[-1],
      function(x) as_monad(eval(x, envir=e), desc=deparse(x))
    ),
    keep_history=keep_history,
    desc=desc
  )

}
# internal function, for building from a list of expressions
.funnel_sub <- function(es, env=parent.frame(), ...){

  ms <- lapply(es, function(x) as_monad(eval(x, env), desc=deparse(x)))

  combine(ms, ...)
}


#' @rdname x_to_monad
#' @export
combine <- function(xs, keep_history=TRUE, desc=NULL){
# combine :: [m *] -> m [*]

  xs <- lapply(xs, as_monad)

  # make a new monad that is the child of all monads in the input list
  out <- new_monad()
  m_parents(out) <- xs

  # store all values (even if failing, in which case should be NULL)
  m_value(out) <- lapply(xs, m_value)

  # monad is passing if all parents are cool
  m_OK(out) <- all(sapply(xs, m_OK))

  if(!is.null(desc)){
    m_code(out) <- desc 
  }

  out 
}



#' Safely builds a list of monads from an argument list of expressions
#'
#' Deprecated, use funnel instead. \code{lsmeval} is the same as \code{funnel},
#' but the name funnel more clearly expresses the use of this function. Which
#' is to join multiple pipes into one.
#'
#' @param ... expressions to be wrapped into monads
#' @param keep_history Merge the histories of all monads
#' @return A list of Rmonads
#' @export
lsmeval <- function(..., keep_history=TRUE){

  .Deprecated("funnel")

  funnel(..., keep_history=keep_history)

}
