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
#' introduce the most useful of these here. For more information see the
#' vignettes.
#'
#' @section Basic Operators:
#'
#'  \%>>\%    monadic bind
#'
#'  \%v>\%    monadic bind and record input
#'
#'  \%>_\%    perorm rhs action, discard result, pass the lhs
#'
#'  \%||\%    if input is error, use rhs value instead
#'
#'  \%|>\%    if input is error, run rhs on last passing result
#'
#'  \code{read.csv("a.csv") \%||\% iris \%>>\% head}
#'
#' @section Advanced operators:
#'
#' These are probably not the operators you are looking for
#'
#'  \%*>\%    bind lhs list as arguments to right
#'
#'  \%^>\%    monadic bind and record input in monad
#'
#'  \%>^\%    bind as a new branch, pass input on main
#'
#'  \%__\%    transfer only history from the lhs (errors ignored)
#'
#'  \%v__\%   like \%__\% but store lhs result
#'
#' @section x to monad functions:
#'
#' as_monad - evaulate an expression into a monad (capturing error)
#'
#' lsmeval - evaluate expressions into a list inside a monad
#'
#' @section monad to monad functions:
#'
#' forget - erase history from a monad
#'
#' doc - add a documentation string to a monad
#'
#' combine - combine a list of monads into a list in a monad
#'
#' @section monad to x functions:
#'
#' esc - extract the result from a computation
#'
#' mtabulate - summarize all steps in a pipeline into a table
#'
#' missues - tabulate all warnings and errors from a pipeline 
#'
#' unstore - get list of all stored intermediate values
#'
#' @docType package
#' @name rmonad
#' @examples
#'
#' # chain operations
#' cars %>>% colSums
#'
#' # chain operations with intermediate storing
#' cars %v>% colSums
#'
#' # handle failing monad
#' iris %>>% colSums %|>% head
#' cars %>>% colSums %|>% head
#'
#' # run an effect
#' cars %>_% plot %>>% colSums
#'
#' # join two independent pipelines, preserving history
#' cars %>>% colSums %v__% cars %>>% lapply(sd) %>>% unlist
#'
#' # load an expression into a monad, catching errors
#' as_monad(stop("instant death"))
#'
#' # convert multiple expressions into a list inside a monad
#' lsmeval(stop("oh no"), runif(5), sqrt(-1))

NULL
