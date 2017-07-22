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
  entry_lhs_transform = entry_lhs_transform_default,
  bind_if             = function(m) m_OK(m),
  bind_else           = function(...){NULL},
  emit                = emit_default,
  m_on_bind           = function(x, ...){x},
  io_combine          = default_combine,
  bind_args           = function(m) list(m_value(m)),
  expect_rhs_function = TRUE
){
  # FIXME: cleanup this implementation
  # FIXME: !!!!!!!

  e <- parent.frame()

  fdecon <- extract_metadata(substitute(f), env=e, skip_name=!expect_rhs_function)
  rhs_str <- deparse(fdecon$expr)
  rhs_doc <- fdecon$docstring
  rhs_met <- fdecon$metadata

  xdecon <- extract_metadata(substitute(x), env=e)
  lhs_str <- deparse(xdecon$expr)
  lhs_doc <- xdecon$docstring
  lhs_met <- xdecon$metadata

  m <- entry_lhs_transform(x, f, desc=lhs_str)

  if(!.has_doc(m)){
    m_doc(m) <- lhs_doc
  }
  if(!.has_meta(m)){
    m_meta(m) <- lhs_met
  }

  o <- if(bind_if(m))
  {

    # insert x as first positional in f
    fs <- substitute(f)
    fl <- as.list(fs)

      bound_args <- bind_args(m)
      final_args <- bound_args

      new_function <- function(){}

      # If the expressions is of form 'x %>>% Foo::bar'
      # Package names are supported fine if arguments are given
      if(fl[[1]] == '::' && length(fl) == 3) {
        new_function <- f
      }
      # Evaluate '.' inside an anonymous function, e.g. 'x %>>% { 2 * . }'
      # If a expanded list is passed, accept keywords
      else if(fl[[1]] == '{'){
        keys <- names(final_args)
        if(is.null(keys)){
          keys <- rep("", length(final_args))
        }
        if(keys[1] == ""){
          keys[1] <- "."
        }
        if(any(keys == "")){
          msg <- "Error in %s: Arguments to an anonymous function must be named"
          stop(msg)
        }
        names(final_args) <- keys

        body(new_function) <- fs
        formals(new_function) <- final_args
      }
      # As in magrittr, fail if an anonymous function is in the pipeline
      # without the parentheses. The infix operators act on the function body
      else if(fl[[1]] == "function"){
        stop("Anonymous functions must be parenthesized", call.=FALSE)
      }
      else if(fl[[1]] == "(" && fl[[2]][[1]] == "function"){
        new_function <- eval(fl[[2]], envir=e)
      }
      else {
        new_function <- eval(fl[[1]], envir=e)
        final_args <- append(bound_args, fl[-1])
      }

    o <- .eval(
      func       = new_function,
      args       = final_args,
      env        = e,
      desc       = rhs_str,
      bound_args = bound_args    # nested histories
    )

    m <- m_on_bind(m)

    io_combine(m, o)

  } else {
    bind_else(m, f)
  }

  if(!is.null(o)){
    m_doc(o)  <- rhs_doc
    m_meta(o) <- rhs_met
    m_code(o) <- rhs_str
  }

  result <- emit(m, o)
  m_mem(result) <- as.integer(object.size(m_value(result)))
  result
}


# Evaluate the expression, load timing info into resultant object
.eval <- function(func, args, env, desc, bound_args){
  st <- system.time(
    {
      o <- as_monad( do.call(func, args, envir=env), desc=desc )
    },
    gcFirst=FALSE # this kills performance when TRUE
  )
  m_time(o) <- signif(unname(st[1]), 2)

  splice_function(func, o, bound_args)
}


## m_on_bind options

# preserve value upon future bind
store_value <- function(m) { .m_stored(m) <- TRUE ; m }

entry_lhs_transform_default <- function(m, f, ...) as_monad(m, ...)

emit_default <- function(input, output) {
  # NOTE: output here is an Rmonad, not a value. It will be NULL only if no
  # bind operation was performed. It may wrap a NULL value.
  if(is.null(output)){
    input
  } else {
    output
  }
}


## io_combine options

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
