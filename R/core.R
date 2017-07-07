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
#' @param entry_lhs_transform f(m,x,...) a transform of the lhs called on entry
#' @param bind_if f(m) bind rhs to lhs if TRUE
#' @param bind_else f(m,f) actio to take if bind_if is FALSE
#' @param emit f(i,o) Emit the input or the output
#' @param m_on_bind f(m) Action to perform on input monad when binding
#' @param bind_args function to retrieve the arguments
#' @param io_combine f(m,o) weave m and f(m) into final output
#' @return A monad report
bind <- function(
  x,
  f,
  entry_lhs_transform = function(m, f, ...) { mrun(m, ...) } ,
  bind_if = function(m) m_OK(m),
  bind_else = toss,
  emit = function(i, o) { if(is.null(o)){ i } else { o } },
  m_on_bind = ident,
  io_combine = default_combine,
  bind_args = function(m) { list(m_value(m)) }
){

  left_str = deparse(substitute(x))
  m <- entry_lhs_transform(x, f, desc=left_str)

  o <- if(bind_if(m))
  {

    # insert x as first positional in f
    fs <- substitute(f)
    fl <- as.list(fs)

    # if the input is of form 'Foo::bar'
    ff <- if(fl[[1]] == '::' && length(fl) == 3) {
      list(as.call(fl)) %++% bind_args(m)
    } else {
      list(fl[[1]]) %++% bind_args(m) %++% fl[-1]
    }

    e <- parent.frame()
    o <- mrun( eval(as.call(ff), envir=e), desc=deparse(fs) )

    m <- m_on_bind(m)

    io_combine(m, o)

  } else {
    bind_else(m, f)
  }

  emit(m, o)
}

branch_combine <- function(m, o){
  m <- app_branch(m, o)
  m
}

default_combine <- function(m, o){
  if(!m_OK(o)){
    # On failure, propagate the final passing value, this allows
    # for either degugging or passage to alternative handlers.
    m_value(o) <- m_value(m)
  }
  m_history(o) <- .make_history(m)
  o
}

bypass_combine <- function(m, o){
  m_value(o) <- m_value(m)
  m_history(o) <- .make_history(m)
  o
}
