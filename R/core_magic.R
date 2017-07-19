rewire <- function(e, m, inputs){

  bound <- get_bound_variables(e, inputs)

  e <- get_function(e)

  # TODO:
  # 1. gather all variables local to the function
  # 2. find local variables that have a bound variable on the rhs (these are dependent)
  # 3. make dependent sets of bound variables
  # 4. extract this environment
  # 5. convert nested pipeline into many nodes
  # 6. this is, of course, done recursively

  # NOTE: be sure to the case of no internal piping

}


gather_all_locals <- function(func, inputs){

  bound <- get_bound_variables(func, inputs)

  get_args(func)

  # TODO:
  #  1. remove args that are bound
  #  2. merge with all declarations in the preamble

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
  expression_filter(get_body(expr), is_assignment_call)
}

get_declarations_lhs <- function(expr){
  expression_filter(get_body(expr), is_assignment_call) %>% lapply('[[', 2)
}

get_declarations_rhs <- function(expr){
  expression_filter(get_body(expr), is_assignment_call) %>% lapply('[[', 3)
}

get_monadic_operators <- function(expr){
  expression_filter(get_body(expr), is_rmonad_operator)
}

is_assignment_call <- function(expr){
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
