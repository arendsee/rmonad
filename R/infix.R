#' Infix operators
#'
#' Infix monadic sequence operators
#'
#'
#' \strong{Monadic bind operators}
#'
#'\tabular{ll}{
#'
#'  \%>>\%    monadic bind
#'
#'  \%v>\%    monadic bind and print input
#'
#'  \%^>\%    monadic bind and record input in monad
#'
#'  \%>^\%    bind as a new branch, pass input on main
#'
#'  \%?>\%    if input is error, run rhs on last passing result
#'
#'  \%<?\%    if input is error, use rhs value instead
#'
#'  \%__\%    ignore the input, replacing the value with the rhs
#'
#'  \%=_\%    assign input to a variable (lfs) and clear
#'
#'  \%=>\%    assign input to a variable (lfs) and pass
#'
#'}
#'
#'\strong{Monadic list operators}
#'
#'\tabular{ll}{
#'
#'  \%,,\%    merges rhs and lhs monads
#'
#'  \%,?\%    if last list element failed, use this one instead
#'
#'  \%,=\%    assign a name the last element in the list
#'
#'  \%,>\%    run rhs function with lhs list of arguments
#'
#'  \%,@\%    curry lhs list of paramters into rhs function 
#'
#'}
#'
#'\strong{Utility operators}
#'
#'\tabular{ll}{
#'
#'  \%.\%     compose two functions (math order)
#'
#'  \%|\%     compose two functions (pipeline order)
#'
#'  \%>\%     pass lhs into rhs (from magrittr)
#'
#'}
#'
#'@param lhs left hand value
#'@param rhs right hand value
#'@name infix
NULL

#' @rdname infix
#' @export
`%$>%` <- function(lhs, rhs) {
    code <- deparse(substitute(lhs))
    lhs <- mrun(lhs, code)
    envir <- parent.frame()
    eval(as.call(list(bind, lhs, substitute(rhs))), envir=envir)
}

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
    cmd   <- list(bind, substitute(lhs), substitute(rhs), record_in=TRUE)
    eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%>^%` <- function(lhs, rhs) {
    envir <- parent.frame()
    cmd   <- list(bind, substitute(lhs), substitute(rhs), branch=TRUE)
    eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%^>%` <- function(lhs, rhs) {
    envir <- parent.frame()

    # combine branches
    x <- combine(lhs@stage@branch, keep_history=FALSE)
    # move history
    x@history <- append(lhs@stage, lhs@history)

    cmd <- list(bind, x, substitute(rhs))
    o <- eval(as.call(cmd), envir=envir)

    # Annotate as a branching function
    o@stage@code <- paste(o@stage@code, " - FUNCTION ON BRANCHES")
    o
}

#' @rdname infix
#' @export
`%>_%` <- function(lhs, rhs) {
    envir <- parent.frame()
    cmd   <- list(bind, substitute(lhs), substitute(rhs), discard_out=TRUE)
    eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%?>%` <- function(lhs, rhs) {
    envir <- parent.frame()
    cmd   <- list(bind, substitute(lhs), substitute(rhs), handle=TRUE)
    eval(as.call(cmd), envir=envir)
}

#' @rdname infix
#' @export
`%?<%` <- function(lhs, rhs) {
    if(lhs@OK){
      lhs
    } else {
      code <- deparse(substitute(rhs))
      rhs <- mrun(rhs, code)
      rhs@history <- append(lhs@history, lhs@stage)
      rhs
    }
}

#' @rdname infix
#' @export
`%__%` <- function(lhs, rhs) {
    code <- deparse(substitute(rhs))
    rhs <- mrun(rhs, code)
    rhs@history <- append(lhs@history, lhs@stage)
    rhs
}

#' @rdname infix
#' @export
`%v__%` <- function(lhs, rhs) {

    code <- deparse(substitute(rhs))
    rhs <- mrun(rhs, code)

    lhs@stage@x <- lhs@x
    rhs@history <- append(lhs@history, lhs@stage)
    rhs
}

#' @rdname infix
#' @export
`%^__%` <- function(lhs, rhs) {
    code <- deparse(substitute(rhs))
    rhs <- mrun(rhs, code)

    # store lhs as a branch
    rhs@stage@branch <- list(lhs)

    rhs
}

# #' @rdname infix
# #' @export
# `%=_%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%=>%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,,%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,,%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,=%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,>%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,@%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%.%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%|%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%>%` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
