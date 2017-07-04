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
#'  \%>v\%    run right-hand for its effects -- bind message, pass value
#'
#'  \%>^\%    like \%>v\% but stores the result in the monad
#'
#'  \%?>\%    if input is error, run rhs on last passing result
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
`%>>%` <- function(lhs, rhs) {
    envir <- parent.frame()
    eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
}


#' @rdname infix
#' @export
`%v>%` <- function(lhs, rhs) {
    envir <- parent.frame()
    cmd   <- list(bind, substitute(lhs), substitute(rhs), print_in=TRUE)
    eval(as.call(cmd), envir=envir)
}

# #' @rdname infix
# #' @export
# `%^>` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir, record_in=TRUE, pass="input")
# }
#
# #' @rdname infix
# #' @export
# `%>v` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir, fmap=TRUE, pass="input")
# }
#
# #' @rdname infix
# #' @export
# `%>^` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%?>` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%__` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%=_` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%=>` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,,` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%, ` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,=` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,>` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%,@` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%.` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%|` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
#
# #' @rdname infix
# #' @export
# `%>` <- function(lhs, rhs) {
#     envir <- parent.frame()
#     eval(as.call(list(bind, substitute(lhs), substitute(rhs))), envir=envir)
# }
