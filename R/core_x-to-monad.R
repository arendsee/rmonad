#' Conversions to monads
#'
#' These functions convert possibly non-monadic inputs into monads.
#'
#' For each of these functions, failure of any part causes failure of the
#' whole. Any non-monadic inputs will be converted to monads. Any exceptions
#' raised in the inputs will be caught.
#'
#' \code{as_monad} evaluate a single expression into an Rmonad. If the value is
#' already an Rmonad, it will be nested.
#'
#' \code{funnel} evaluates multiple arguments into one Rmonad. It can be used
#' within pipelines to create multi-input nodes (works well with \code{\%*>\%}).
#'
#' \code{combine} takes a list of Rmonads and joins the elements into one
#' Rmonad. The values of the original monadic containers joined into a list in
#' the child Rmonad. The list Rmonads are recorded as the new Rmonad's parents.
#'
#' @param expr An expression
#' @param xs  A list of elements to join into a monad
#' @param doc A docstring to associate with the monad
#' @param desc A description of the monad (usually the producing code)
#' @param clone logical Should the R6 object be cloned?
#' @param keep_history Merge the histories of all monads
#' @param env Evaluation environment
#' @param lossy logical Should unnesting with record be done?
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
as_monad <- function(expr, desc=NULL, doc=NULL, lossy=FALSE, clone=FALSE){
# as_monad :: a -> m a

  value <- NULL 
  warns <- NULL
  fails <- NULL
  isOK  <- TRUE

  env <- parent.frame()
  ed <- extract_metadata(substitute(expr), env=env)
  expr <- ed$expr
  doc <- ed$docstring
  met <- ed$metadata

  st <- system.time(
    {
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
              warns <<- warns %++% w$message
              invokeRestart("muffleWarning")
            }
          )
        },
        type="message"
      )
    },
    gcFirst=FALSE # this kills performance when TRUE
  )

  if(lossy && is_rmonad(value)){
    if(clone){
      value <- value$clone()
    }
    return(value)
  }

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  m <- Rmonad$new()

  # The default value is Nothing
  if(isOK) m_value(m) <- value

  # These accessors do the right thing (don't mess with them)
  m_code(m)     <- code
  m_error(m)    <- fails
  m_warnings(m) <- warns
  m_notes(m)    <- notes
  m_OK(m)       <- isOK
  m_doc(m)      <- doc
  m_mem(m)      <- object.size(value)
  m_time(m)     <- signif(unname(st[1]), 2)
  m_meta(m)     <- met

  m

}


#' @rdname x_to_monad
#' @export
funnel <- function(..., env=parent.frame(), keep_history=TRUE){
# funnel :: [Rexpr] -> m [*]

  # NOTE: don't deparse '...' to get labels, this leads to massive performance
  # penalties when data is piped in (e.g. deparsing a dataframe).

  ms <- .funnel_ms(
    es = substitute(alist(...))[-1],
    env=env
  )

  desc <- deparse(match.call())

  combine(ms, keep_history=keep_history, desc=desc)

}

# internal function, for building from a list of expressions
.funnel_sub <- function(es, env=parent.frame(), ...){

  ms <- .funnel_ms(es, env)

  combine(ms, ...)
}
.funnel_ms <- function(es, env=parent.frame()){
  lapply(
    es,
    # how to stringify x
    function(x) {

      # if x is a call, deparse it
      desc <- if(is.call(x)){
        deparse(x)
      }
      # the substitution in funnel will evaluated these
      else if(is.atomic(x) && length(x) == 1) {
        as.character(x)
      }
      else if(is.name(x)){
        as.character(x)
      }
      # anything else, will be data passed in from the pipe
      else {
        "."
      }

      as_monad(eval(x, env), desc=desc, lossy=TRUE)
    }
  )
}


#' @rdname x_to_monad
#' @export
combine <- function(xs, keep_history=TRUE, desc=NULL){
# combine :: [m *] -> m [*]

  if(!all(sapply(xs, is_rmonad))){
    stop("'combine' works only on lists of Rmonad objects")
  }

  # make a new monad that is the child of all monads in the input list
  out <- Rmonad$new()
  m_parents(out) <- xs

  # store all values (even if failing, in which case should be NULL)
  m_value(out) <- lapply(xs, m_value, warn=FALSE)

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
