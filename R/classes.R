#' A record of past events 
#'
#' @slot x        A list of 0 or 1 elements, containing either nothing or data
#' @slot code     A string showing the function that created record's report 
#' @slot error    An error in this this record
#' @slot warnings List of warnings
#' @slot notes    List of notes
#' @slot doc      character vector documentation messages
#' @slot branch   Lost of connected Rmonad objects
record <- setClass(
  "record",
  representation(
    x        = "list",      # list just to support polymorphism, should always be of length 1
    code     = "character",
    error    = "character",
    warnings = "list",
    notes    = "list",
    doc      = "character",
    branch   = "list"
  ),
  prototype(
    x        = list(),
    code     = "",
    error    = "",
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
