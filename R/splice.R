rmonad_ops <- c(
  "%>>%",
  "%v>%",
  "%>_%",
  "%||%",
  "%|>%",
  "%*>%",
  "%__%",
  "%v__%",
  "%>^%",
  "%^>%"
)

#' Take a monadic bind operation's result and splice histories
#'
#' We need to link input variables to the nodes in the nested pipeline that use
#' them.
#'
#' @param f The function
#' @param m The monadic result of running f(ms)
#' @param ms The list of inputs passed to f
#' @param ... additional arguments passed to add_transitive_edges
splice_function <- function(f, m, ms, ...){

  ops <- get_monadic_operators(f)

  if(length(ops) == 0)
    return(m)

  bv <- get_bound_variables(f, ms)

  decs <- get_declarations(get_preamble(f))

  deps <- get_dependency_matrix(decs, names(bv))

  add_transitive_edges(m=m, bv=bv, deps=deps, ...)

}

#' Find inputs to a nest
#'
#' @param m The current monadic node
#' @param bv A named list of bound variables
#' @param deps A mapping local variables to bound variable dependencies
#' @keywords internal
add_transitive_edges <- function(m, bv, deps, final, parent){

  code <- lapply(get_code(m), parse_as_block)
  free_all <- lapply(code, get_free_variables)
  free_locals <- lapply(free_all, function(x) x[ x %in% dimnames(deps)[[1]] ])
  dependencies <- lapply(
    free_locals,
    function(x) {
      bv[deps[x, , drop=FALSE] %>% colSums %>% '>'(0)]
    }
  ) %>% lapply(unname)

  transitive_edges <- list()

  if(length(dependencies) > 0){
    for(child_id in seq_along(dependencies)){
      for(parent_id in dependencies[[child_id]]){
        final <- .connect(
          final,
          from = parent_id,
          to   = child_id + size(parent),
          type = 'transitive'
        )
      }
    }
  }

  final 
}

parse_as_block <- function(code_str){
  # make a vector of expressions
  expr <- parse(text=code_str)
  # If there is only one expression, return this expression
  if(length(expr) == 1){
    expr[[1]]
  # Otherwise wrap the expressions in a block
  } else {
    as.call(c(as.name('{'), as.list(expr)))
  }
}


#' Get dependencies of local variables on inputs
#'
#' @param declarations A list of declarations
#' @param bound_vars Character vector of variables names that are bound as
#' arguments to the function
#' @return logical matrix
get_dependency_matrix <- function(declarations, bound_vars){

  lhs <- lapply(declarations, get_lhs) %>% lapply(as.character) %>% unlist 
  rhs <- lapply(declarations, get_rhs)
  rhs_free_vars <- lapply(rhs, get_free_variables)

  deps <- bound_vars
  names(deps) <- bound_vars

  all_vars <- unique(c(bound_vars, lhs))

  deps <- matrix(
    data=FALSE,
    nrow=length(all_vars),
    ncol=length(bound_vars),
    dimnames = list(all_vars, bound_vars)
  )
  for(v in bound_vars){
    deps[v,v] <- TRUE
  }

  vars <- bound_vars
  for(i in seq_along(declarations)){

    varname <- lhs[i]
    rfv <- rhs_free_vars[[i]]

    # Only consider the free variables that are declared or imported in the header.
    # This allows globals or NSE variables (e.g. `subset(cars, dist > 10)`)
    rfv <- rfv[rfv %in% rownames(deps)]

    if(length(rfv) == 0){
      deps[varname, ] <- rep(FALSE, ncol(deps))
    } else if(length(rfv) == 1){
      deps[varname, ] <- deps[rfv, , drop=FALSE]
    } else {
      deps[varname, ] <- deps[intersect(vars, rfv), , drop=FALSE] %>% colSums %>% '>'(0)
    }

  }

  deps

}


# Find variables that are used within an expression but that are not locally bound.
#
# In calls, the function name is ignored.
#
# '.' is magic, being automatically bound.
#
get_free_variables <- function(expr, bound_args=""){

  # For functions, append all parameter names to the list of bound variables
  # (these are not free), and then recurse into the body
  if(is_function(expr)){
    names(get_args(expr)) %>%
      append(bound_args) %>%
      get_free_variables(expr=get_body(expr))
  }

  # If the expression is an access to a value inside a variable, the return the
  # accessed variable. For example, the expression `x$y` contains only the one
  # free variable, `x`, not two. Note that the expression `x[[y]]`, contains
  # two free variables.
  else if(is.call(expr) && (as.character(expr[[1]]) %in% c('$', '@'))){
    as.character(expr[[2]])
  }

  # For declarations, append the defined variable to bound list, then recurse
  # into the right-hand side
  else if(is_declaration(expr)) {
    get_lhs(expr) %>%
      append(bound_args) %>%
      get_free_variables(expr=get_rhs(expr))
  }

  # Return a name is it is not bound
  else if(is.name(expr) && ! (as.character(expr) %in% bound_args)){
    as.character(expr)
  }

  # If this is a code block, recruse into each expression. If they are
  # assignments, record the newly bound variable.
  # NOTE: '{' is also a call, so must preceded `else if(is.call(expr))`
  else if(
    is.call(expr) &&
    expr[[1]] == '{'
  ){
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

  # If any of the magrittr or Rmonad operators are in the expression, then '.'
  # will be automatically bound downstream.
  # On the lhs, '.' is global; on the rhs, '.' is bound.
  # in: `5 %>% add(.)`   -- no free variables
  # in: `. %>% add(.)`   -- '.' is a free variable
  else if(
    is.call(expr) &&
    any(as.character(expr[[1]]) %in% c('%>%', '%<>%', '%T>%', '%$%', rmonad_ops))
  ){
    rhs_freevars <- get_free_variables(expr[[2]], bound_args=bound_args)
    bound_args <- c(bound_args, '.')
    lhs_freevars <- get_free_variables(expr[[3]], bound_args=bound_args)
    unique(c(rhs_freevars, lhs_freevars))
  }

  # For a call, recurse into each argument
  else if(is.call(expr)){

    lapply(expr[-1], get_free_variables, bound_args) %>% unlist %>% unique
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
  } else if(e[[1]] == '(' && e[[2]][[1]] == 'function'){
    e[[2]]
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
  class(expr) == "call" && any(as.character(expr) %in% rmonad_ops)
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
