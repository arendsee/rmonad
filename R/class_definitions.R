#' The eponymous monad type
#'
#' @slot x        A list of 0 or 1 elements, containing either nothing or data
#' @slot OK       logical Is the monad passing?
#' @slot id       integer A unique ID for the object
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
    id       = "integer",
    code     = "character",
    error    = "list", # Maybe [String]
    warnings = "list", # Maybe [String]
    notes    = "list", # Maybe [String]
    doc      = "list", # Maybe [String]
    other    = "list",
    branch   = "list",
    parents  = "list",
    .stored  = "logical"
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
    branch   = list(),
    .stored  = FALSE, # is an x stored here
    parents  = list()
  )
)


.__indexed_monad__ <- function(){
  rmonad_id <- 1L
  function(){
    r <- Rmonad()
    r@id <- rmonad_id 
    rmonad_id <<- rmonad_id + 1L
    r
  }
}
# create a record with a global id
new_rmonad <- .__indexed_monad__()
