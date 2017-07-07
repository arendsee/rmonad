#' Run an expression, capture EWM, return Rmonad
#'
#' If the value is already an Rmonad, the existing value is returned.
#'
#' @param expr An expression
#' @param desc A name to assign to the code slot
#' @return Rmonad object 
#' @export
mrun <- function(expr, desc=NULL){
# TODO: rename to as_monad

  value <- NULL 
  warns <- NULL
  fails <- NULL
  isOK  <- TRUE

  notes <- capture.output(
    {
      value <- withCallingHandlers(
        tryCatch(
          expr,
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

  if(class(value) == "Rmonad") { return(value) }

  if(!isOK) {
    value = NULL
  }

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  m <- new_rmonad()

  # These accessors do the right thing (don't mess with them)
  m_value(m)    <- value
  m_code(m)     <- code
  m_error(m)    <- fails
  m_warnings(m) <- warns
  m_notes(m)    <- notes
  m_OK(m)       <- isOK

  m

}


#' Safely builds a list of monads from a list of data
#'
#' @export
#' @param ... expressions to be wrapped into monads
#' @param keep_history Merge the histories of all monads
#' @return A list of Rmonads
#' @examples
#' lsmeval( list(1:10, stop(1)) )
lsmeval <- function(..., keep_history=TRUE){

  instr <- sprintf("lsmeval(%s)",
    lapply(substitute(alist(...))[-1], deparse) %>% unlist %>% paste0(collapse=", "))

  ll <- as.list(substitute(alist(...)))[-1]

  ms <- lapply(ll, function(x) mrun(eval(x), desc=deparse(x)))

  combine(ms, keep_history=keep_history, desc=instr)

}

# internal function, for building from a list of expressions
.lsmeval_sub <- function(es, env=parent.frame(), ...){

  ms <- lapply(es, function(x) mrun(eval(x, env), desc=deparse(x)))

  combine(ms, ...)
}


#' Takes a list of monads and joins them
#'
#' If any of the monads are failing, the resulting one will also
#'
#' Any non-monadic values will be converted to monads. However, combine will
#' NOT catch errors. To safely build monadic lists, use lsmeval.
#'
#' @param ms  A list of monads
#' @param keep_history Merge the histories of all monads
#' @param desc A description of the monad (usually the producing code)
combine <- function(ms, keep_history=TRUE, desc=NULL){

  ms <- lapply(ms, mrun)

  rec <- .indexed_record()

  history <- if(keep_history) {
    lapply(ms, .make_history)
  } else {
    list()
  }

  out <- new(
     "Rmonad",
     x        = list(),
     stage    = rec,
     history  = history,
     OK       = FALSE
  )

  # store all values (even if failing, in which case should be NULL)
  m_value(out) <- lapply(ms, m_value)

  if(all(sapply(ms, m_OK))){
    m_OK(out) <- TRUE
  }

  if(!is.null(desc)){
    m_code(out) <- desc 
  }

  out 
}
