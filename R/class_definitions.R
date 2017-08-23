reset_rmonad_id <- function(){
  .rmonad_node_id=0L
  function(){
    .rmonad_node_id <<- .rmonad_node_id + 1 
    .rmonad_node_id
  }
}
.generate_node_id <- reset_rmonad_id()



#' Create an Rmonad object
#'
#' @description This is the R monad that holds any state information relevant to the thread
#' of pure computation. 
#'
#' @field value      The computed value the node wraps
#' @field id         The node's unique id
#' @field OK         A logical storing whether the node is in a passing or failing state
#' @field code       A string showing the function that created record's report 
#' @field error      Character vector of length 1, an error message
#' @field warnings   Character vector of warnings (if any)
#' @field notes      Character vector of notes (if any). Notes include anything raised with \code{message}.
#' @field doc        Character vector of length 1, the node's docstring
#' @field other      List of metadata generated at runtime (currently includes time and space)
#' @field meta       List of metadata about the function specified in a list after the docstring
#' @field branch     List of child pipelines dependent on this node but independent of the main pipe
#' @field parents    List of parent Rmonad objects, these are the node's inputs
#' @field nest       A nested pipeline that produced this nodes value given this nodes inputs
#' @field nest_depth The nesting depth of this node (with 1 being unnested)
#'
#'
#' @section Getters:
#'
#' \describe{
#'   \item{\code{get_value}}{Get \code{value}. Raise a warning if no value is cached.}
#'   \item{\code{get_id}}{Get \code{id}}
#'   \item{\code{get_OK}}{Get \code{OK}}
#'   \item{\code{get_code}}{Get \code{code}}
#'   \item{\code{get_error}}{Get \code{error}}
#'   \item{\code{get_warnings}}{Get \code{warnings}}
#'   \item{\code{get_notes}}{Get \code{notes}}
#'   \item{\code{get_doc}}{Get \code{doc}}
#'   \item{\code{get_other}}{Get \code{other}}
#'   \item{\code{get_meta}}{Get \code{meta}}
#'   \item{\code{get_branch}}{Get \code{branch}}
#'   \item{\code{get_parents}}{Get \code{parents}}
#'   \item{\code{get_nest}}{Get \code{nest}}
#'   \item{\code{get_nest_depth}}{Get \code{nest_depth}}
#'   \item{\code{get_time}}{Get \code{time}. This is a value stored in the field \code{other}}
#'   \item{\code{get_mem}}{Get \code{mem}. This is a value stored in the field \code{other}}
#'   \item{\code{get_stored}}{Get \code{stored}}
#' }
#'
#' @section Setters:
#'
#' \describe{
#'   \item{\code{set_value}}{Set \code{value}}
#'   \item{\code{set_id}}{Set \code{id}. Raises a warning if an id already is defined.}
#'   \item{\code{set_OK}}{Set \code{OK}}
#'   \item{\code{set_code}}{Set \code{code}}
#'   \item{\code{set_error}}{Set \code{error}}
#'   \item{\code{set_warnings}}{Set \code{warnings}. See also \code{app_warnings}}
#'   \item{\code{set_notes}}{Set \code{notes}. See also \code{app_nodes}}
#'   \item{\code{set_doc}}{Set \code{doc}}
#'   \item{\code{set_other}}{Set \code{other}}
#'   \item{\code{set_meta}}{Set \code{meta}}
#'   \item{\code{set_branch}}{Set \code{branch}. See also \code{app_branch}}
#'   \item{\code{set_parents}}{Set \code{parents}. See also \code{app_parents}}
#'   \item{\code{set_nest}}{Set \code{nest}}
#'   \item{\code{set_nest_depth}}{Set \code{nest_depth}}
#'   \item{\code{set_time}}{Set \code{time}}
#'   \item{\code{set_mem}}{Set \code{mem}}
#'   \item{\code{set_stored}}{Set \code{stored}}
#' }
#'
#' @section Other accessors:
#'
#' \describe{
#'   \item{\code{app_warnings}}{Append list to \code{app_warnings}}
#'   \item{\code{app_notes}}{Append list to \code{app_notes}}
#'   \item{\code{app_branch }}{Append list to \code{app_branch }}
#'   \item{\code{app_parents}}{Append list to \code{app_parents}}
#' }
#'
#' \describe{
#'   \item{\code{has_code}}{Check whether code{code} exists}
#'   \item{\code{has_error}}{Check whether code{error} exists}
#'   \item{\code{has_doc}}{Check whether code{doc} exists}
#'   \item{\code{has_warnings}}{Check whether code{warnings} exists}
#'   \item{\code{has_notes}}{Check whether code{notes} exists}
#'   \item{\code{has_parents}}{Check whether code{parents} exists}
#'   \item{\code{has_nest}}{Check whether code{nest} exists}
#'   \item{\code{has_branch}}{Check whether code{branch} exists}
#'   \item{\code{has_meta}}{Check whether code{meta} exists}
#'   \item{\code{has_time}}{Check whether code{time} exists}
#'   \item{\code{has_mem}}{Check whether code{mem} exists}
#'   \item{\code{has_value}}{Check whether code{value} exists}
#' }
#'
#' @section Other methods:
#'
#' \describe{
#'   \item{\code{delete_value}}{Remove the value from this node. This is NOT
#'   the same as assigning NULL to the \code{x} field, since NULL is a valid
#'   value that may be stored as the result of the node's computation.
#'   Internally, Rmonad stores a value as a list, with 0 or 1 elements, where
#'   the optional element is the stored value. This allows NULL to be
#'   distingushed from an uncached value.}
#'   \item{\code{inherit}}{Link a node to a single or list of parent Rmonad
#'   objects.}
#' }
#'
#' @docType class
#'
#' @format An \code{\link{R6Class}} factory
#' @usage # m <- Rmonad$new()
#' @name Rmonad_cls
Rmonad <- R6::R6Class(
  "Rmonad",
  public = list(
    value      = list(), # Maybe a
    OK         = TRUE,
    code       = NA_character_,
    error      = list(), # Maybe [String]
    warnings   = list(), # Maybe [String]
    notes      = list(), # Maybe [String]
    doc        = list(), # Maybe [String]
    other      = list(),
    meta       = list(),
    branch     = list(), # TODO: recast as 'children', no special firstborn treatment
    parents    = list(),
    nest       = list(),
    nest_depth = NA_integer_,
    initialize = function(){
      private$set_id()
    },

    get_value = function(warn=TRUE) {
      if(warn && length(self$value) == 0){
        warning("Attempting to access the value of a non-cached node, returning NULL")
      }
      private$maybe_vector_get(self$value)
    },
    get_id         = function() private$id,
    get_OK         = function() self$OK,
    get_code       = function() self$code,
    get_error      = function() self$error,
    get_warnings   = function() private$maybe_vector_get(self$warnings),
    get_notes      = function() private$maybe_vector_get(self$notes),
    get_doc        = function() private$maybe_vector_get(self$doc),
    get_other      = function() self$other,
    get_meta       = function() self$meta,
    get_branch     = function() self$branch,
    get_parents    = function() private$maybe_vector_get(self$parents),
    get_nest       = function() private$maybe_vector_get(self$nest),
    get_nest_depth = function() self$nest_depth,
    get_time = function() {
      time <- self$other$time
      if(is.null(time)){
        NA_real_
      } else {
        time
      }
    },
    get_mem = function() {
      mem <- self$other$mem
      if(is.null(mem)){
        NA_real_
      } else {
        mem
      }
    },
    get_stored = function() private$stored,

    # TODO: Add checking to all of these
    set_value      = function(x) self$value       <- list(value=x),
    set_OK         = function(x) self$OK          <- x,
    set_code       = function(x) self$code        <- x,
    set_error      = function(x) self$error       <- as.character(x),
    set_warnings   = function(x) self$warnings    <- private$maybe_vector_set(x, private$is_not_empty_string, expected_type=is.character),
    set_notes      = function(x) self$notes       <- private$maybe_vector_set(x, private$is_not_empty_string, expected_type=is.character),
    set_doc        = function(x) self$doc         <- private$maybe_vector_set(x, private$is_not_empty_string, expected_type=is.character),
    set_other      = function(x) self$other       <- x,
    set_meta       = function(x) self$meta        <- x,
    set_branch     = function(x) self$branch      <- x,
    set_parents    = function(x) self$parents     <- private$maybe_vector_set(x, private$is_not_empty),
    set_nest       = function(x) self$nest        <- private$maybe_vector_set(x, private$is_not_empty),
    set_nest_depth = function(x) self$nest_depth  <- x,
    set_time       = function(x) self$other$time  <- x,
    set_mem        = function(x) self$other$mem   <- x,
    set_stored     = function(x) private$stored   <- x,

    app_warnings = function(x) {
      if(length(x) > 0 && nchar(x) > 0){
        self$warnings <- x %++% self$warnings
      }
    },
    app_notes = function(x) {
      if(length(x) > 0 && nchar(x) > 0){
        self$notes <- x %++% self$notes
      }
    },
    app_branch  = function(x) self$branch  <- x %++% self$branch,
    app_parents = function(x) self$parents <- x %++% self$parents,

    has_code     = function() private$is_not_empty_string(self$code)  ,
    has_error    = function() length(self$error)    != 0              ,
    has_doc      = function() length(self$doc)      != 0              ,
    has_warnings = function() length(self$warnings) != 0              ,
    has_notes    = function() length(self$notes)    != 0              ,
    has_parents  = function() length(self$parents)  != 0              ,
    has_nest     = function() length(self$nest)     != 0              ,
    has_branch   = function() length(self$branch)   != 0              ,
    has_meta     = function() length(self$meta)     != 0              ,
    has_time     = function() private$is_not_empty_real(self$time)    ,
    has_mem      = function() private$is_not_empty_integer(self$mem)  ,
    has_value    = function() length(self$value) == 1                 ,

    delete_value = function() {
      self$value <- list() # Nothing
      private$stored <- FALSE
    },

    inherit = function(
      parents,
      inherit_value = FALSE,
      inherit_OK    = FALSE,
      force_keep    = FALSE
    ) {

      .rm_value_if <- function(m, force_keep=FALSE){
        if(!force_keep && !.m_stored(m)){
          m <- m_delete_value(m)
          .m_stored(m) <- FALSE
        } else {
          .m_stored(m) <- TRUE
        }
        m
      }

      if(is_rmonad(parents)){
        if(inherit_value){
          self$set_value(parents$get_value())
        }
        if(inherit_OK){
          self$OK <- parents$OK
        }
        parents <- .rm_value_if(parents, force_keep=force_keep)
        self$set_parents(list(parents))
      } else {
        if(inherit_value){
          self$set_value(lapply(parents, m_value))
        }
        if(inherit_OK){
          self$OK <- all(lapply(parents, m_OK))
        }
        parents <- lapply(parents, .rm_value_if, force_keep=force_keep)
        self$set_parents(parents)
      }
    }


  ),

  private = list(
    id = NA_integer_,
    stored = FALSE, # is an x stored here
    set_id = function(){
      if(!is.na(private$id)){
        warning("Changing the id of a node is usually a very bad idea")
      }
      private$id <- .generate_node_id()
    },

# === A note about Maybe ======================================================
# Some of the values stored in Rmonad could reasonably contain nothing, which
# is not the same as NULL. The value a node wraps be anything. But intermediate
# values are not usually stored (unless using %v>% or relatives), so we need a
# way to distinguish between a node holding no value and NULL. My approach is
# to emulate the Haskell Maybe by using a list of length 0 or 1. An empty list
# is Nothing. A list with one element, is Something.

    is_not_empty = function(x) length(x) > 0,

    is_not_empty_string = function(x) {
      !is.null(x)     &&
      !is.na(x)       &&
      is.character(x) &&
      (
        length(x) > 1 ||
        (length(x) == 1 && nchar(x) > 0)
      )
    },

    is_not_empty_integer = function(x) {
      !is.null(x) && !is.na(x) && is.integer(x) && length(x) != 0
    },

    is_not_empty_real = function(x) {
      !is.null(x) && !is.na(x) && is.numeric(x) && length(x) != 0
    },

    maybe_vector_get = function(x){
      if(length(x) == 0){
        NULL   # Nothing
               # NOTE: this is still ambiguous
      } else {
        x$value # a
      }
    },

    maybe_vector_set = function(x, is_not_empty, expected_type=true){
      if(is_not_empty(x)){
        if(!expected_type(x)){
          stop("Type error")
        }
        list(value=x) # Just a
      } else {
        list()  # Nothing
      }
    }
  )
)
