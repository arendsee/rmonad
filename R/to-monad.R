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
#' @param keep_history merge the histories of all monads
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
as_monad <- function(expr, desc=.default_code(), doc=.default_doc(), lossy=FALSE){
# TODO: 'lossy' is an lousy name, should change to 'nest', or something
# as_monad :: a -> m a

  value <- .default_value()
  warns <- .default_warnings()
  fails <- .default_error()
  isOK  <- .default_OK()

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
    return(value)
  }

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  m <- Rmonad()

  if(isOK){
    .single_value(m) <- value
  } else {
    m <- .set_raw_value(m, voidCache())
  }

  # These accessors do the right thing (don't mess with them)
  .single_code(m)       <- code
  .single_error(m)      <- fails
  .single_warnings(m)   <- warns
  .single_notes(m)      <- notes
  .single_OK(m)         <- isOK
  .single_doc(m)        <- doc
  .single_mem(m)        <- object.size(value)
  .single_time(m)       <- signif(unname(st[1]), 2)
  .single_meta(m)       <- met
  .single_summary(m)    <- .default_summary()

  m <- apply_rewriters(m, met)

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
combine <- function(xs, keep_history=TRUE, desc=.default_code()){
# combine :: [m *] -> m [*]

  if(!all(sapply(xs, is_rmonad))){
    stop("'combine' works only on lists of Rmonad objects")
  }

  # make a new monad that is the child of all monads in the input list
  out <- Rmonad() 

  # store all values (even if failing, in which case should be NULL)
  .single_value(out) <- lapply(xs, .single_value, warn=FALSE)

  xs <- lapply(xs, .single_delete_value)

  .single_parents(out) <- xs

  # monad is passing if all parents are cool
  .single_OK(out) <- all(sapply(xs, .single_OK))

  if(!is.null(desc)){
    .single_code(out) <- desc 
  }

  out 
}
