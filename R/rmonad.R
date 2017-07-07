#' @importFrom methods isClass new
#' @importFrom utils capture.output object.size
#' @importFrom methods show
#' @importFrom magrittr "%>%"
utils::globalVariables(c("%>%", "."))
NULL

#' rmonad: handling pipes, errors, and everything with monads
#'
#' The Rmonad package consists mainly of a set of monadic bind operators for
#' controlling a pipeline and handling error. It also contains functions for
#' operating on monads and evaluating expressions into monads. I will briefly
#' introduce all of these here. For more information see the vignettes.
#'
#' @section Operators:
#'
#'  \%>>\%    monadic bind
#'
#'  \%*>\#    bind lhs list as arguments to right
#'
#'  \%v>\%    monadic bind and record input
#'
#'  \%^>\%    monadic bind and record input in monad
#'
#'  \%>^\%    bind as a new branch, pass input on main
#'
#'  \%>_\%    perorm rhs action, discard result, pass the lhs
#'
#'  \%|>\%    if input is error, run rhs on last passing result
#'
#'  \%||\%    if input is error, use rhs value instead
#'
#'  \%__\%    ignore the input, replacing the value with the rhs
#'
#'  \%^__\%   ignore the input, replacing the value with the rhs
#'
#' @section x to monad functions:
#'
#' lsmeval
#'
#' mrun
#'
#' @section monad to monad functions:
#'
#' forget
#'
#' doc
#'
#' combine
#'
#' @section monad to x functions:
#'
#' mtabulate
#'
#' missues
#'
#' esc
#'
#' un*
#'
#' @docType package
#' @name rmonad
#' @examples
#'
#' 5 %>>% sqrt %>>% sqrt
NULL
