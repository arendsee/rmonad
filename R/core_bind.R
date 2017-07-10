#' Apply f to the contents of a monad and merge messages 
#'
#' This function should not be used directly. Rather you should use the infix
#' operators. They all wrap this function.
#'
#' Non-standard evaluation to insert x into f as the first positional argument.
#' This allows specialization of f, but also prevents higher-order voodoo from
#' being performed.
#'
#' @export
#' @param x The input, may or may not be a monad report
#' @param f A function of the value contained in x
#' @param entry_lhs_transform f(m,x,...) a transform of the lhs called on entry
#' @param bind_if f(m) bind rhs to lhs if TRUE
#' @param bind_else f(m,f) action to take if bind_if is FALSE
#' @param emit f(i,o) Emit the input or the output
#' @param m_on_bind f(m) Action to perform on input monad when binding
#' @param bind_args function to retrieve the arguments
#' @param io_combine f(m,o) weave m and f(m) into final output
#' @keywords internal
#' @return A monad report
bind <- function(
  x,
  f,
  entry_lhs_transform = function(m, f, ...) as_monad(m, ...),
  bind_if             = function(m) m_OK(m),
  bind_else           = toss,
  emit                = emit_default,
  m_on_bind           = ident,
  io_combine          = default_combine,
  bind_args           = function(m) list(m_value(m))
){
  # FIXME: cleanup this implementation

  left_str = deparse(substitute(x))
  m <- entry_lhs_transform(x, f, desc=left_str)

  o <- if(bind_if(m))
  {
    e <- parent.frame()

    # insert x as first positional in f
    fs <- substitute(f)
    fl <- as.list(fs)

      # If the expressions is of form 'x %>>% Foo::bar'
      # Package names are supported fine if arguments are given
      expr <- if(fl[[1]] == '::' && length(fl) == 3) {
        as.call( list(as.call(fl)) %++% bind_args(m) )
      }
      # Evaluate '.' inside an anonymous function, e.g. 'x %>>% { 2 * . }'
      else if(fl[[1]] == '{'){
        a_function <- eval(call("function", as.pairlist(alist(. =)), fs), envir=e)
        e = environment()
        as.call( list(quote(a_function), .=m_value(m)) )
      }
      else {
        as.call( list(fl[[1]]) %++% bind_args(m) %++% fl[-1] )
      }

    st <- system.time(
      {
        o <- as_monad( eval(expr, envir=e), desc=deparse(fs) )
      },
      gcFirst=FALSE # this kills performance when TRUE
    )
    m_time(o) <- signif(unname(st[1]), 2)

    m <- m_on_bind(m)

    io_combine(m, o)

  } else {
    bind_else(m, f)
  }

  o <- emit(m, o)
  m_mem(o) <- as.integer(object.size(m_value(o)))
  o
}

emit_default <- function(i , o) {
  if(is.null(o)){
    i
  } else {
    o
  }
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

  .m_inherit(child=o, parents=m)
}

bypass_combine <- function(m, o){
  # the new value inherits the old value, losing whatever it had
  # but the pass/fail state of the child is preserved
  .m_inherit(child=o, parents=m, inherit_value=TRUE, inherit_OK=FALSE)
}
