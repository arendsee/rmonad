#' The eponymous monad type
#'
#' @slot x        A list of 0 or 1 elements, containing either nothing or data
#' @slot OK       logical Is the monad passing?
#' @slot code     A string showing the function that created record's report 
#' @slot error    An error in this this record
#' @slot warnings Character vector of warnings
#' @slot notes    Character vector of notes
#' @slot doc      character vector documentation messages
#' @slot other    list of other things (currently includes time and space)
#' @slot parents  list of parent Rmonad objects
#' @slot .stored  logical (internal) whether an x value should be kept
Rmonad <- setClass(
  "Rmonad",
  representation(
    x        = "list", # Maybe a
    OK       = "logical",
    code     = "character",
    error    = "list", # Maybe [String]
    warnings = "list", # Maybe [String]
    notes    = "list", # Maybe [String]
    doc      = "list", # Maybe [String]
    other    = "list",
    meta     = "list",
    branch   = "list",
    parents  = "list",
    .stored  = "logical"
  ),
  prototype(
    x        = list(),
    OK       = TRUE,
    code     = NA_character_,
    error    = list(),
    warnings = list(),
    notes    = list(),
    doc      = list(),
    other    = list(),
    meta     = list(),
    branch   = list(),
    .stored  = FALSE, # is an x stored here
    parents  = list()
  )
)
