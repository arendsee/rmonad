#' Infix operators
#'
#' Infix monadic sequence operators
#'
#' See the main package help page (\code{?rmonad}) or the \code{intro} and
#' \code{cheatsheet} vignettes for more information.
#'
#'@param lhs left hand value
#'@param rhs right hand value
#'@name infix
NULL

#' @rdname infix
#' @export
`%>>%` <- function(lhs, rhs) {
  envir <- parent.frame()
  eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
}

#' @rdname infix
#' @export
`%v>%` <- function(lhs, rhs) {
  envir <- parent.frame()
  cmd <- list(bind, substitute(lhs), substitute(rhs), m_on_bind=store_value)
  eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%*>%` <- function(lhs, rhs) {
  # m [*] -> ([*] -> m b) -> m b
  envir <- parent.frame()

  lhs_expr <- substitute(lhs)

  # FIXME: This is really sneaky. I am ignoring x and y and replacing them with
  # variables defined in this foreign environment. Not good practice.
  if(is.call(lhs_expr) && lhs_expr[[1]] == "list"){
    ninja_desc <- deparse(lhs_expr)
    lhs_expr <- lhs_expr[-1]
    on_entry <- function(x, f, ...) {
      ninja_desc
      .funnel_sub(
        lhs_expr,
        env=envir,
        keep_history = TRUE,
        desc         = ninja_desc
      )
    }
  } else {
    on_entry <- entry_lhs_transform_default
  }

  cmd   <- list(
    bind,
    lhs_expr,
    substitute(rhs),
    bind_args=m_value,
    parent_ids=function(m) m_parents(m),
    entry_lhs_transform=on_entry
  )
  eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%>_%` <- function(lhs, rhs) {
  envir <- parent.frame()
  cmd   <- list(
    bind,
    substitute(lhs),
    substitute(rhs),
    io_combine = bypass_combine
  )
  eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%>^%` <- function(lhs, rhs) {
  envir <- parent.frame()
  cmd   <- list(bind, substitute(lhs), substitute(rhs), io_combine=branch_combine)
  eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%^>%` <- function(lhs, rhs) {
  .bind_if_fb <- function(m){
    all( ms_OK(m)[m_children(m)] )
  }
  .bind_args_fb <- function(m){
    ms_value(m, warn=FALSE)[m_children(m)]
  }

  cmd   <- list(
    bind,
    lhs,
    substitute(rhs),
    bind_args = .bind_args_fb,
    bind_if   = .bind_if_fb
  )
  envir <- parent.frame()
  eval(as.call(cmd), envir=envir)

}

#' @rdname infix
#' @export
`%|>%` <- function(lhs, rhs) {
  envir <- parent.frame()
  bind_if <- function(m) { ! m_OK(m) }
  cmd   <- list(bind, substitute(lhs), substitute(rhs), bind_if=bind_if)
  eval(as.call(cmd), envir=envir)
}


# ----------------------------------------------------
# The following have values, not functions, on the rhs
# ----------------------------------------------------

#' @rdname infix
#' @export
`%||%` <- function(lhs, rhs) {
  envir <- parent.frame()
  rhs_str <- deparse(substitute(rhs))

  emit <- function(input,output) {
    # if the lhs failed, pass the evaluated rhs
    if(m_OK(input)){
      input
    }
    # else link the rhs to lhs input, and replace the lhs
    else {
      inherit(child=output, parent=input)
    }
  }

  cmd <- list(
    bind,
    substitute(lhs),
    substitute(rhs),
    # skip the bind code
    bind_if = false,
    # and instead just evaluate the rhs
    bind_else = function(i,o) as_monad(o, desc=rhs_str),
    emit = emit,
    expect_rhs_function = FALSE
  )
  eval(as.call(cmd), envir=envir)
}



#' @rdname infix
#' @export
`%__%` <- function(lhs, rhs) {

  envir <- parent.frame()
  .chain(substitute(lhs), substitute(rhs), envir)

}

#' @rdname infix
#' @export
`%v__%` <- function(lhs, rhs) {

  .Deprecated("Use `%__%` instead, they now do the same thing")

  envir <- parent.frame()
  .chain(substitute(lhs), substitute(rhs), envir)

}

.chain <- function(lhs, rhs, envir) {

  emit <- function(i,o) {
    inherit(
      child         = o,
      parent        = i,
      type          = "prior",
      inherit_value = FALSE,
      inherit_OK    = FALSE,
      force_keep    = TRUE
    )
  }

  cmd <- list(
    bind,
    lhs,
    rhs,
    bind_if   = false,
    bind_else = function(...){as_monad(eval(rhs, envir), lossy=TRUE)},
    emit      = emit,
    expect_rhs_function = FALSE,
    envir=envir
  )
  eval(as.call(cmd), envir=envir)
}
