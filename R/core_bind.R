#' Apply f to the contents of a monad and merge messages 
#'
#' This function should not be used directly. Rather you should use the infix
#' operators. They all wrap this function.
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
  bind_args           = function(m) list(m_value(m, warn=FALSE)),
  parent_ids          = function(m) m_id(m),
  expect_rhs_function = TRUE,
  envir               = parent.frame()
){
  # FIXME: cleanup this implementation

  fdecon <- extract_metadata(substitute(f), env=envir, skip_name=!expect_rhs_function)
  rhs_str <- deparse(fdecon$expr)
  rhs_doc <- fdecon$docstring
  rhs_met <- fdecon$metadata

  xdecon <- extract_metadata(substitute(x), env=envir)
  lhs_str <- deparse(xdecon$expr)
  lhs_doc <- xdecon$docstring
  lhs_met <- xdecon$metadata

  m <- entry_lhs_transform(x, f, desc=lhs_str)

  if(!has_doc(m)){
    m_doc(m) <- lhs_doc
  }
  if(!has_meta(m)){
    m_meta(m) <- lhs_met
  }

  o <- if(bind_if(m))
  {
    fs <- fdecon$expr
    fl <- as.list(fs)

      bound_args <- bind_args(m)
      final_args <- bound_args

      # If the expressions is of form 'x %>>% Foo::bar'
      # No special handling needed if arguments are given
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

        new_function <- pryr::make_function(
          .as_positional_formals(names(final_args)),
          fs,
          env=envir
        )

        rhs_str <- deparse(new_function)
      }
      # As in magrittr, fail if an anonymous function is in the pipeline
      # without the parentheses. The infix operators act on the function body
      else if(fl[[1]] == "function"){
        stop("Anonymous functions must be parenthesized", call.=FALSE)
      }
      else if(fl[[1]] == "(" && fl[[2]][[1]] == "function"){
        new_function <- eval(fl[[2]], envir=envir)
      }
      else {
        new_function <- eval(fl[[1]], envir=envir)
        final_args <- append(bound_args, fl[-1])
      }

    o <- .eval(
      func       = new_function,
      args       = final_args,
      env        = envir
    )

    m <- m_on_bind(m)

    o <- io_combine(m=m, o=o, f=new_function, margs=parent_ids(m))

    apply_rewriters(o, rhs_met)

  } else {
    bind_else(m, f)
  }

  # NOTE: This causes much pain. It is a hack I wrote for reasons I've
  # forgotten. There should be a more natural place to set this info. This
  # sometimes overwrites previous settings creating the most subtle bugs.
  if(!is.null(o)){
    m_doc(o)  <- rhs_doc
    m_meta(o) <- rhs_met
    m_code(o) <- rhs_str
  }

  result <- emit(m, o)
  m_mem(result) <- as.integer(object.size(m_value(result, warn=FALSE)))
  result
}


# FIXME: Find a better way to do this. I need to replace a list of names with
# an `alist` of unnamed (positional) arguments.
.as_positional_formals <- function(arg_names){
  code_str <- sprintf("alist(%s)", paste0(arg_names, " = ", collapse=", ")) 
  eval(parse(text=code_str))
}

# Evaluate the expression, load timing info into resultant object
.eval <- function(func, args, env){

  st <- system.time(
    {
      result <- as_monad( do.call(func, args, envir=env)) %>% unnest
    },
    gcFirst=FALSE # this kills performance when TRUE
  )
  m_time(result) <- signif(unname(st[1]), 2)

  result
}


## m_on_bind options

# preserve value upon future bind
store_value <- function(m) { .m_stored(m) <- TRUE ; m }

entry_lhs_transform_default <- function(m, f, ...) {
  # FIXME: This is a sneaky way of safely evaluating the lhs without nesting
  # the nads. I need a cleaner solution.
  as_monad(m, lossy=TRUE, ...)
}

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

branch_combine <- function(m, o, f, margs){
  # Add o as a normal child of m, preserving its value
  o <- inherit(child=o, parent=m, type='depend', force_keep=TRUE)
  # Point head to the parent
  o@head <- m@head
  o
}

default_combine <- function(m, o, f, margs){
  if(has_nest(o)){
    o <- splice_function(f=f, m=o, ms=margs)
  }
  inherit(child=o, parent=m, inherit_value=!m_OK(o))
}

bypass_combine <- function(m, o, f, margs){
  # the new value inherits the old value, losing whatever it had but the
  # pass/fail state of the child is preserved
  inherit(child=o, parent=m, inherit_value=TRUE, inherit_OK=FALSE)
}
