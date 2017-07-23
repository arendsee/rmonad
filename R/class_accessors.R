#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @name rmonad_accessors
NULL

# === A note about Maybe ======================================================
# Some of the values stored in Rmonad could reasonably contain nothing, which
# is not the same as NULL. The value a node wraps be anything. But intermediate
# values are not usually stored (unless using %v>% or relatives), so we need a
# way to distinguish between a node holding no value and NULL. My approach is
# to emulate the Haskell Maybe by using a list of length 0 or 1. An empty list
# is Nothing. A list with one element, is Something.

# FIXME: I'm doing this wrong. Attempting to access a value that does not exist
# should raise an error. Setting a value to NULL emphatically should not be the
# same as unstoring a value. There needs to be special functions for deleting a
# value. The current code passes my tests. But is logically wrong.

.is_not_empty <- function(x) length(x) > 0

.is_not_empty_string <- function(x) {
  !is.null(x)     &&
  !is.na(x)       &&
  is.character(x) &&
  (
    length(x) > 1 ||
    (length(x) == 1 && nchar(x) > 0)
  )
}

.is_not_empty_integer <- function(x) {
  !is.null(x) && !is.na(x) && is.integer(x) && length(x) != 0
}

.is_not_empty_real <- function(x) {
  !is.null(x) && !is.na(x) && is.numeric(x) && length(x) != 0
}

.maybe_vector_get <- function(x){
  if(length(x) == 0){
    NULL   # Nothing
           # NOTE: this is still ambiguous
  } else {
    x[[1]] # a
  }
}

.maybe_vector_set <- function(x, is_not_empty, expected_type=true){
  if(is_not_empty(x)){
    if(!expected_type(x)){
      stop("Type error")
    }
    list(x) # Just a
  } else {
    list()  # Nothing
  }
}

#' @rdname rmonad_accessors
#' @export
m_delete_value <- function(m) {
  m@value <- list() # Nothing
  m
}

# === END Maybe ===============================================================


#' @rdname rmonad_accessors
#' @export
is_rmonad <- function(m) {
  class(m) == "Rmonad"
}


# These are currently only internal accessors, that provide a sane interface to
# the all the things that may be construed as empty
.has_code     <- function(m) .is_not_empty_string(m_code(m))
.has_error    <- function(m) length(m_error(m))    != 0
.has_doc      <- function(m) length(m_doc(m))      != 0
.has_warnings <- function(m) length(m_warnings(m)) != 0
.has_notes    <- function(m) length(m_notes(m))    != 0
.has_parents  <- function(m) length(m_parents(m))  != 0
.has_branch   <- function(m) length(m_branch(m))   != 0
.has_meta     <- function(m) length(m_meta(m))     != 0
.has_time     <- function(m) .is_not_empty_real(m_time(m))
.has_mem      <- function(m) .is_not_empty_integer(m_mem(m))
.has_value    <- function(m) .m_stored(m) || !is.null(m_value(m))

#' @rdname rmonad_accessors
#' @export
m_parents <- function(m) .maybe_vector_get(m@parents)

#' @rdname rmonad_accessors
#' @export
m_value <- function(m) .maybe_vector_get(m@x)

#' @rdname rmonad_accessors
#' @export
m_id <- function(m) m@id

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) m@OK

#' @rdname rmonad_accessors
#' @export
m_code <- function(m) m@code

#' @rdname rmonad_accessors
#' @export
m_error <- function(m) .maybe_vector_get(m@error)

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) .maybe_vector_get(m@warnings)

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m) .maybe_vector_get(m@notes)

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m) .maybe_vector_get(m@doc)

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m) m@meta

#' @rdname rmonad_accessors
#' @export
m_time <- function(m) {
  time <- m@other$time
  if(is.null(time)){
    NA_real_
  } else {
    time
  }
}


#' @rdname rmonad_accessors
#' @export
m_mem <- function(m) {
  mem <- m@other$mem
  if(is.null(mem)){
    NA_real_
  } else {
    mem 
  }
}

#' @rdname rmonad_accessors
#' @export
m_branch   <- function(m) m@branch

#' @rdname rmonad_accessors
#' @export
`m_id<-` <- function(m, value) {
  m@id <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  m@OK <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  m@x <- .maybe_vector_set(value, .is_not_empty)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  m@parents <- .maybe_vector_set(value, .is_not_empty)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  m@code <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  m@error <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  m@warnings <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  m@notes <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  m@doc <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  m@meta <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  m@other$time <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  m@other$mem <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_branch<-` <- function(m, value) {
  m@branch <- value
  m
}



#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value) {
  if(length(value) > 0 && nchar(value) > 0){
    m_warnings(m) <- value %++% m_warnings(m)
  }
  m
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value) {
  if(length(value) > 0 && nchar(value) > 0){
    m_notes(m) <- value %++% m_notes(m)
  }
  m
}

#' @rdname rmonad_accessors
#' @export
app_branch <- function(m, value) {
  m_branch(m) <- value %++% m_branch(m)
  m
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  m_parents(m) <- value %++% m_parents(m)
  m
}
