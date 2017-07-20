# splice_funcion <- function(e, m, ms, envir=parent.frame()){
#
#   bound <- get_bound_variables(e, ms)
#
#   func <- get_function(e)
#
#   e <- get_local_environment(func, names(bound), envir)
#
#   # 1. gather all variables local to the function
#   # 2. find local variables that have a bound variable on the rhs (these are dependent)
#   # 3. make dependent sets of bound variables
#   # 4. extract this environment
#   # 5. convert nested pipeline into many nodes
#   # 6. this is, of course, done recursively
#
#   # NOTE: be sure to the case of no internal piping
#
# }


# Find variables that are used within an expression but that are not locally bound.
#
# In calls, the function name is ignored.
#
# '.' is magic, being automatically bound.
#
get_free_variables <- function(expr, bound_args=c(".")){

  # For functions, append all parameter names to the list of bound variables
  # (these are not free), and then recurse into the body
  if(is_function(expr)){
    names(get_args(expr)) %>%
      append(bound_args) %>%
      get_free_variables(expr=get_body(expr))
  }

  # For declarations, append the defined variable to bound list, then recurse
  # into the right-hand side
  else if(is_declaration(expr)) {
    get_lhs(expr) %>%
      append(bound_args) %>%
      get_free_variables(expr=get_rhs(expr))
  }

  # If this is a code block, recruse into each expression. If they are
  # assignments, record the newly bound variable.
  # NOTE: '{' is also a call, so must preceded `else if(is.call(expr))`
  else if(expr == '{'){
    freevars <- character(0)
    for(e in as.list(expr)[-1]){
      if(is_declaration(e)){
        bound_args <- c(bound_args, as.character(get_lhs(e)))
        e <- get_rhs(e)
      }
      freevars <- c(freevars, get_free_variables(e, bound_args))
    }
    unique(freevars)
  }

  # For a call, recurse into each argument
  else if(is.call(expr)){
    lapply(expr[-1], get_free_variables, bound_args) %>% unlist %>% unique
  }

  # Return a name is it is not bound
  else if(is.name(expr) && ! (as.character(expr) %in% bound_args)){
    as.character(expr)
  }

  # Otherwise, return an empty vector
  else {
    character(0)
  }
}

get_rhs <- function(expr){
  .get_hand(expr, function(x) x[[3]])
}
get_lhs <- function(expr){
  .get_hand(expr, function(x) x[[2]])
}
.get_hand <- function(expr, hand){
  if(is.call(expr)){
    if(length(expr) != 3)
      stop("Can only get side for binary operators")
    hand(expr)
  } else {
    stop("Can only get lhs of a call")
  }
}


# get_dependent_variables <- function(func, bound_vars){
#
#   # Get top-level declarations
#   decs <- expression_filter(
#     get_preamble(func),
#     keep_cond = is.name,
#     desc_cond = false
#   )
#
# }

# given `x = f(y)`:
#  1.  - x is on 



get_implicitly_passed_variables <- function(func){

}


# Map the names of variables in a function to an input list. The main purpose
# is to check for mismatches and give explicit names to positional arguments.
get_bound_variables <- function(e, ms){

  if(length(ms) == 0)
    return(list())

  e <- get_args(e)

  # expression and monadic list labels
  el <- names(e)
  ml <- names(ms)

  # number of elements in each
  ne <- length(e)
  nm <- length(ms)

  if(is.null(ml)){
    if(ne < nm)
      stop("To many arguments passed function")
    ml <- el[seq_len(nm)]
  } else {
    # ms elements that have no names
    nameless <- is.na(ml) | is.null(ml) | nchar(ml) == 0
    pos <- setdiff(el, ml)
    ml[nameless] <- pos[seq_len(sum(nameless))]
  }

  if(length(setdiff(ml, el)) != 0)
    stop("The parameter list has arguments not present in the function")

  names(ms) <- ml

  ms
}

get_function <- function(e){
  if(is.function(e)){
    e 
  } else if(is.call(e)){
    if(e[[1]] != "function")
      stop("Expected call to be a function expression")
    eval(e)
  } else {
    stop("This doesn't seem to be a function")
  }
}

is_function <- function(e){
  is.function(e) ||
  (
    is.call(e) &&
    e[[1]] == "function"
  )
}

get_args <- function(e) {
  formals(get_function(e))
}

# Similar to `body` function, but also works for functions as calls. If it is
# given a block, it just returns the block.
get_body <- function(e) {
  if(class(e) == '{')
    return(e)

  body(get_function(e))
}

# Everything declared on the right of a rmonad operator is local. Data is only
# passed through sanctioned channels. This is because right hand elements are
# either functions or values enclosed in brackets. So any local variables in a
# function, must be in the preamble, preceding the first monadic operator. This
# function extracts that preamble.
get_preamble <- function(expr){
  bod <- get_body(expr)
  preamble <- list()   
  i = 0
  for(e in as.list(bod)){
    if(is_rmonad_operator(e))
      break
    i <- i + 1
  }
  bod[1:i]
}

get_declarations <- function(expr){
  expression_filter(get_body(expr), is_declaration)
}

get_declarations_lhs <- function(expr){
  expression_filter(get_body(expr), is_declaration) %>% lapply('[[', 2)
}

get_declarations_rhs <- function(expr){
  expression_filter(get_body(expr), is_declaration) %>% lapply('[[', 3)
}

get_monadic_operators <- function(expr){
  expression_filter(get_body(expr), is_rmonad_operator)
}

is_declaration <- function(expr){
  class(expr) %in% c("=", "<-")
}

is_rmonad_operator <- function(expr){
  class(expr) == "call" &&
    any(as.character(expr) %in%
      c("%>>%", "%v>%", "%>_%", "%||%", "%|>%", "%*>%", "%__%", "%v__%", "%>^%", "%^>%")
    )
}

expression_filter <- function(expr, keep_cond=true, desc_cond=true){
  keepers <- if(keep_cond(expr)){
    list(expr) 
  } else {
    list()
  }

  if(desc_cond(expr) && class(expr) %in% c("{", "call")){
    append(keepers, unlist(lapply(expr, expression_filter, keep_cond, desc_cond)))
  } else {
    keepers
  }
}


# # Rule 1
# #
# # ...  %>>% f(x)
# # ... %>% funnel(x=x) %*>% { f(x) }
#
# function(x) { f %>>% g %>>% h }
#
# %>>% function(x) { f(c) %>>% g(x) %>>% h(x) }
#
#
#  -> M1 ; ...... %>>% g(x) %>>%
#
#  -> M1 ; ...... %>% funnel(x=M1) %*>% g %>>%
#
#
#  -> M1 ; f(c) %>% funnel(x=M1) %*>% g %>% funnel(x=M1) %*>% h
#
#
# '%>>% g(x, ...)'  -->  '%>% funnel(x=M1) %*>% g(...)'
#
#
#   %>>% function(x) { f(c) %>>% g(x) %>>% h(x) }
#   -------v--------        ----v---- ----v----
#       -> M1 ;        f(c)
#                         %>% funnel(x=M1) %*>% g
#                                 %>% funnel(x=M1) %*>% h
#
#
#
#   %>>% function(x) { f(c) %>>% g(x) %>>% h(x) }
#   --------v-------
#     %>% function(x=.) { f(c) %>% funnel(x=.) %*>% g %>% funnel(x=.) %*>% h }
#
# rhs cases:
#  1. named function
#  2. anonymous function
#  3. braced function
#  4. partially applied named function
#  5. partially applied anonymous function
#
# lhs cases:
#  1. single input
#  2. multiple inputs (funnel)
#  3. no input
#
# complications:
#  1. scope

# #' Collect all local variables within a function
# #'
# #' This can be used as the execution environment for all the functional pieces
# #' we build from the disembowled function.
# #'
# #' It counts variables that are defined within the function as well as
# #' arguments that use default values. It does not count arguments are that
# #' passed external values. Passed arguments are listed in the `inputs` list.
# #'
# #' @param func a function or function call
# #' @param bound_args a list of named inputs
# #' @param envir the environment to inherit from
# #' @examples
# #'
# #'
# e = get_local_environment(
#   func=get_local_environment,
#   bound_args=c("func", "inputs"),
#   envir=parent.frame()
# )
# get_local_environment <- function(func, bound_args, envir=parent.frame()){
#   all_args <- get_args(func)
#   local_args <- all_args[! names(all_args) %in% bound_args]
#   e <- list2env(local_args, envir=envir)
#   for(expr in as.list(get_preamble(func))[-1]){
#     eval(expr, envir=e)
#   }
#   e
# }


