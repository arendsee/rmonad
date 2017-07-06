#' Apply f to the contents of a monad and merge messages 
#'
#' This function should not usually be used directly. Rather you should use the
#' infix operators. They all wrap this function.
#'
#' This function uses non-standard evaluation to insert x into f as the first
#' positional argument. This allows specialization of f, but also prevents
#' higher-order voodoo from being performed.
#'
#' @export
#' @param x The input, may or may not be a monad report
#' @param f A function of the value contained in x
#' @param on_entry f(m,x) an action to perform on entry, returns m
#' @param bind_if f(m) bind rhs to lhs if TRUE
#' @param emit f(i,o) Emit the input or the output
#' @param m_on_bind f(m) Action to perform on input monad when binding
#' @param record_in logical - should the input be recorded?
#' @param branch Store the output in the branch slot
#' @param discard_out Ignore the result of the rhs function, passon the left
#' @param discard_in Ignore the input
#' @param handle  Operate on a failed state
#' @return A monad report
bind <- function(
  x,
  f,
  on_entry    = ident,
  bind_if     = function(m) m_OK(m),
  emit        = function(i, o) { if(is.null(o)){ i } else { o } },
  m_on_bind   = ident,
  branch      = FALSE,
  discard_out = FALSE,
  discard_in  = FALSE,
  handle      = FALSE
){

  left_str = deparse(substitute(x))
  m <- mrun(x, desc=left_str)

  m <- on_entry(m, f)

  o <- if(bind_if(m))
  {

    # insert x as first positional in f
    fs <- substitute(f)
    fl <- as.list(fs)

    # if the input is of form 'Foo::bar'
    ff <- if(fl[[1]] == '::' && length(fl) == 3) {
      list(as.call(fl)) %+% m_value(m)
    } else {
      list(fl[[1]]) %+% m_value(m) %++% fl[-1]
    }

    envir <- parent.frame()
    o <- mrun( eval(as.call(ff), envir), desc=deparse(fs) )

    m <- m_on_bind(m)

    if(branch){
      m <- app_branch(m, o)
    } else {
      if(!m_OK(o) || discard_out){
        # On failure, propagate the final passing value, this allows
        # for either degugging or passage to alternative handlers.
        m_value(o) <- m_value(m)
      }
      m_history(o) <- m@history %+% m@stage
    }

    o
  } else { NULL }

  emit(m, o)
}


#' Run an expression, capture EWM, return Rmonad
#'
#' If the value is already an Rmonad, the existing value is returned.
#'
#' @param expr An expression
#' @param desc A name to assign to the code slot
#' @return Rmonad object 
#' @export
mrun <- function(expr, desc=NULL){

  value <- NULL 
  warns <- list()
  fails <- ""
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

  value <- if(isOK) { list(value) } else { list() }

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  new("Rmonad",
    x = value,
    stage = new("record",  
      x        = list(),
      code     = code,
      errors   = fails,
      warnings = as.list(warns),
      notes    = as.list(notes)
    ),
    history = list(),
    OK = isOK
  )

}


#' Merge list of reports into one
#'
#' @export
#' @param ms  List of report
#' @param keep_history Merge the histories of all monads
#' @return A combined report
combine <- function(ms, keep_history=TRUE){
  ms <- lapply(ms, mrun)
  rec <- new("record")
  history <- if(keep_history) {
    Reduce( append, lapply(ms, function(m) append(m@history, m@stage)), list() )
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
  if(all(sapply(ms, function(m) m@OK))){
    out@x  <- Reduce( append, lapply(ms, function(m) m@x), list() )
    out@OK <- TRUE
  }
  out 
}
