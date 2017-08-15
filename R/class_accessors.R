#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @param warn Warn if the accessed field does not exist (value was not cached)
#' @name rmonad_accessors
NULL

# === A note about Maybe ======================================================
# Some of the values stored in Rmonad could reasonably contain nothing, which
# is not the same as NULL. The value a node wraps be anything. But intermediate
# values are not usually stored (unless using %v>% or relatives), so we need a
# way to distinguish between a node holding no value and NULL. My approach is
# to emulate the Haskell Maybe by using a list of length 0 or 1. An empty list
# is Nothing. A list with one element, is Something.

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
    x$value # a
  }
}

.maybe_vector_set <- function(x, is_not_empty, expected_type=true){
  if(is_not_empty(x)){
    if(!expected_type(x)){
      stop("Type error")
    }
    list(value=x) # Just a
  } else {
    list()  # Nothing
  }
}

#' @rdname rmonad_accessors
#' @export
m_delete_value <- function(m) {
  if(is_rmonad(m)){
    m@x <- list() # Nothing
    m@.stored <- FALSE
  }
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
.has_nest     <- function(m) length(m_nest(m))     != 0
.has_branch   <- function(m) length(m_branch(m))   != 0
.has_meta     <- function(m) length(m_meta(m))     != 0
.has_time     <- function(m) .is_not_empty_real(m_time(m))
.has_mem      <- function(m) .is_not_empty_integer(m_mem(m))
.has_value    <- function(m) length(m@x) == 1


# internal utility for generating error messages when accessing a non-Rmonad
.m_check <- function(m) {
  if(!is_rmonad(m)){
    msg="Expected an Rmonad object, got %s"
    stop(sprintf(msg, class(m)))
  }
}

#' @rdname rmonad_accessors
#' @export
m_parents <- function(m) {
  .m_check(m)
  .maybe_vector_get(m@parents)
}

#' @rdname rmonad_accessors
#' @export
m_nest <- function(m) {
  .m_check(m)
  .maybe_vector_get(m@nest)
}

#' @rdname rmonad_accessors
#' @export
m_nest_depth <- function(m) {
  .m_check(m)
  m@nest_depth
}

#' @rdname rmonad_accessors
#' @export
m_value <- function(m, warn=TRUE){
  .m_check(m)
  if(warn && length(m@x) == 0){
    warning("Attempting to access the value of a non-cached node, returning NULL")
  }
  .maybe_vector_get(m@x)
}

#' @rdname rmonad_accessors
#' @export
m_id <- function(m) {
  .m_check(m)
  m@id
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) {
  .m_check(m)
  m@OK
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m) {
  .m_check(m)
  m@code
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m) {
  .m_check(m)
  .maybe_vector_get(m@error)
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) {
  .m_check(m)
  .maybe_vector_get(m@warnings)
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m) {
  .m_check(m)
  .maybe_vector_get(m@notes)
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m) {
  .m_check(m)
  .maybe_vector_get(m@doc)
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m) {
  .m_check(m)
  m@meta
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m) {
  .m_check(m)
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
  .m_check(m)
  mem <- m@other$mem
  if(is.null(mem)){
    NA_real_
  } else {
    mem 
  }
}

#' @rdname rmonad_accessors
#' @export
m_branch   <- function(m) {
  .m_check(m)
  m@branch
}

#' @rdname rmonad_accessors
#' @export
`m_id<-` <- function(m, value) {
  .m_check(m)
  m@id <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  .m_check(m)
  m@OK <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  .m_check(m)
  m@x <- list(value=value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  .m_check(m)
  m@parents <- .maybe_vector_set(value, .is_not_empty)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_nest<-` <- function(m, value) {
  .m_check(m)
  m@nest <- .maybe_vector_set(value, .is_not_empty)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_nest_depth<-` <- function(m, value) {
  .m_check(m)
  m@nest_depth <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .m_check(m)
  m@code <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .m_check(m)
  m@error <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .m_check(m)
  m@warnings <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .m_check(m)
  m@notes <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .m_check(m)
  m@doc <- .maybe_vector_set(value, .is_not_empty_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .m_check(m)
  m@meta <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  .m_check(m)
  m@other$time <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  .m_check(m)
  m@other$mem <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_branch<-` <- function(m, value) {
  .m_check(m)
  m@branch <- value
  m
}



#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value) {
  .m_check(m)
  if(length(value) > 0 && nchar(value) > 0){
    m_warnings(m) <- value %++% m_warnings(m)
  }
  m
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value) {
  .m_check(m)
  if(length(value) > 0 && nchar(value) > 0){
    m_notes(m) <- value %++% m_notes(m)
  }
  m
}

#' @rdname rmonad_accessors
#' @export
app_branch <- function(m, value) {
  .m_check(m)
  m_branch(m) <- value %++% m_branch(m)
  m
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .m_check(m)
  m_parents(m) <- value %++% m_parents(m)
  m
}
