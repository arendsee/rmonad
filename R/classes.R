#' A record of past events 
#'
#' @slot x        A list of 0 or 1 elements, containing either nothing or data
#' @slot code     A string showing the function that created record's report 
#' @slot errors   List of errors accumulated so far 
#' @slot warnings List of warnings accumulated so far
#' @slot notes    List of notes accumulated so far
#' @slot doc      character vector documentation message
#' @slot branch   Lost of connected Rmonad objects
record <- setClass(
  "record",
  representation(
    x        = "list",
    code     = "character",
    errors   = "list",
    warnings = "list",
    notes    = "list",
    doc      = "character",
    branch   = "list"
  ),
  prototype(
    x        = list(),
    code     = "",
    errors   = list(),
    warnings = list(),
    notes    = list(), 
    doc      = "",
    branch   = list()
  )
)

#' The eponymous monad type
#'
#' @slot x       List of values produced if the parent computation succeeded
#' @slot stage   The active record 
#' @slot history A list of past records
#' @slot OK      TRUE if the report is currently passing
Rmonad <- setClass(
  "Rmonad",
  representation(
    x       = "list",
    stage   = "record",
    history = "list",
    OK      = "logical"
  ),
  prototype(
    x       = list(),
    stage   = new("record"),
    history = list(),
    OK      = TRUE
  )
)
