#' @importFrom methods isClass new
#' @importFrom utils capture.output object.size
#' @importFrom methods show
#' @importFrom magrittr "%>%"
utils::globalVariables(c("%>%", "."))
NULL

#' rmonad: handling pipes, errors, and everything with monads
#'
#' Rmonad merges blocks of code into a graph containing the history of all past
#' operations, and optionally their values. It consists mainly of a set of
#' monadic bind operators for controlling a pipeline and handling error. It
#' also contains functions for operating on monads, evaluating expressions into
#' monads, and extracting values from them. I will briefly introduce the most
#' useful of these here. For more information see the vignette.
#'
#' @section Basic Operators:
#'
#'  \%>>\%    monadic bind: applies rhs function to the lhs value
#'
#'  \%v>\%    monadic bind: store intermediate result
#'
#'  \%>_\%    perform rhs action, discard result, pass the lhs
#'
#'  \%||\%    if input is error, use rhs value instead
#'
#'  \%|>\%    if input is error, run rhs on last passing result
#'
#' @section Advanced operators:
#'
#'  \%*>\%    bind lhs list as arguments to right. The lhs may be a literal
#'            list or a monad bound list.
#'
#'  \%^>\%    monadic bind and record input in monad
#'
#'  \%>^\%    bind as a new branch, pass input on main
#'
#'  \%__\%    keep parents from the lhs (errors ignored). This allows chaining
#'            of independent operations.
#'
#'  \%v__\%   like \%__\% but store lhs result
#'
#' @section x to monad functions:
#'
#' as_monad - evaluate an expression into a monad (capturing error)
#'
#' funnel - evaluate expressions into a list inside a monad
#'
#' @section monad to monad functions:
#'
#' forget - erase parents from a monad
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
#' unbranch - extract all branches from the pipeline
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
#' # return first successful operation
#' read.csv("a.csv") %||% iris %>>% head
#'
#' # join two independent pipelines, preserving history
#' cars %>>% colSums %v__% cars %>>% lapply(sd) %>>% unlist
#'
#' # load an expression into a monad, catching errors
#' as_monad(stop("instant death"))
#'
#' # convert multiple expressions into a list inside a monad
#' funnel(stop("oh no"), runif(5), sqrt(-1))

NULL
