#' Infix operators
#'
#' Infix monadic sequence operators
#'
#' This page contains a list of all operators, including experimental ones. See
#' the main package help page (\code{?rmonad}) for a discussion of the
#' operators that are really supported.
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
  cmd <- list(bind, substitute(lhs), substitute(rhs), m_on_bind=.store_value)
  eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%*>%` <- function(lhs, rhs) {
  envir <- parent.frame()
  lexp <- as.list(substitute(lhs))[-1]
  on_entry <- function(x, f, desc) {
    .lsmeval_sub(
      lexp,
      env=envir,
      keep_history = TRUE,
      desc         = paste(desc)
    )
  }
  cmd   <- list(
    bind,
    substitute(lhs),
    substitute(rhs),
    bind_args=m_value,
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
    m_branch(m) %>% sapply(m_OK) %>% all
  }
  .bind_args_fb <- function(m){
    m_branch(m) %>% lapply(m_value)
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
  cmd   <- list(
    bind,
    substitute(lhs),
    substitute(rhs),
    # skip the bind code
    bind_if = .false,
    # and instead just evaluate the rhs
    bind_else = function(i,o) mrun(o, desc=rhs_str),
    # if the lhs failed, pass the evaluated rhs
    emit = function(i,o) if(m_OK(i)){ i } else { o }
  )
  eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%__%` <- function(lhs, rhs) {
  code <- deparse(substitute(rhs))
  rhs <- mrun(rhs, code)
  m_history(rhs) <- .make_history(lhs)
  rhs
}

#' @rdname infix
#' @export
`%v__%` <- function(lhs, rhs) {
  code <- deparse(substitute(rhs))
  rhs <- mrun(rhs, code)
  lhs@stage@x <- lhs@x
  m_history(rhs) <- .make_history(lhs)
  rhs
}
