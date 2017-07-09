#' A record of past events 
#'
#' @slot x        A list of 0 or 1 elements, containing either nothing or data
#' @slot code     A string showing the function that created record's report 
#' @slot error    An error in this this record
#' @slot warnings Character vector of warnings
#' @slot notes    Character vector of notes
#' @slot doc      character vector documentation messages
#' @slot branch   Lost of connected Rmonad objects
record <- setClass(
  "record",
  representation(
    x        = "list", # Maybe a
    OK       = "logical",
    id       = "integer",
    code     = "character",
    error    = "list", # Maybe [String]
    warnings = "list", # Maybe [String]
    notes    = "list", # Maybe [String]
    doc      = "list", # Maybe [String]
    other    = "list",
    branch   = "list"
  ),
  prototype(
    x        = list(),
    OK       = TRUE,
    id       = -1L,
    code     = NA_character_,
    error    = list(),
    warnings = list(),
    notes    = list(),
    doc      = list(),
    other    = list(),
    branch   = list()
  )
)


.__indexed_record__ <- function(){
  rmonad_id <- 1L
  function(){
    r <- record()
    r@id <- rmonad_id 
    rmonad_id <<- rmonad_id + 1L
    r
  }
}
# create a record with a global id
.indexed_record <- .__indexed_record__()


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
    history = "list"
  )
)

# This is the only constructor that should be used
# TODO: add arguments, for now I just use m_* functions
new_rmonad <- function(){
  new("Rmonad",
    x       = list(),
    stage   = .indexed_record(),
    history = list()
  )
}
