#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @name rmonad_accessors

NULL


.is_valid_string <- function(x) {
  !is.null(x)     &&
  !is.na(x)       &&
  is.character(x) &&
  (
    length(x) > 1 ||
    (length(x) == 1 && nchar(x) > 0)
  )
}

.is_valid_integer <- function(x) {
  !is.null(x) && !is.na(x) && is.integer(x) && length(x) != 0
}

.is_valid_real <- function(x) {
  !is.null(x) && !is.na(x) && is.numeric(x) && length(x) != 0
}

.has_code     <- function(m) .is_valid_string(m_code(m))
.has_error    <- function(m) length(m_error(m))    != 0
.has_doc      <- function(m) length(m_doc(m))      != 0
.has_warnings <- function(m) length(m_warnings(m)) != 0
.has_notes    <- function(m) length(m_notes(m))    != 0
.has_parents  <- function(m) length(m_parents(m))  != 0
.has_branch   <- function(m) length(m_branch(m))   != 0
.has_time     <- function(m) .is_valid_real(m_time(m))
.has_mem      <- function(m) .is_valid_integer(m_mem(m))
.has_value    <- function(m) .m_stored(m) || !is.null(m_value(m))

.maybe_vector_get <- function(x){
  if(length(x) == 0){
    NULL   # Nothing
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

.m_stored <- function(m) {
  m@.stored
}
`.m_stored<-` <- function(m, value) { m@.stored <- value ; m }
# preserve value upon future bind
.store <- function(m) { .m_stored(m) <- TRUE ; m }

.rm_value_if <- function(m, force_keep=FALSE){
  if(!force_keep && !.m_stored(m)){
    m_value(m) <- NULL
    .m_stored(m) <- FALSE
  } else {
    .m_stored(m) <- TRUE
  }
  m
}

# Function is central to bind. It determines how past context is passed to the
# new value.
.m_inherit <- function(
  child,
  parents,
  inherit_value = FALSE,
  inherit_OK    = FALSE,
  force_keep    = FALSE
) {
  if(class(parents) == "Rmonad"){
    if(inherit_value)
      m_value(child) <- m_value(parents)
    if(inherit_OK)
      m_OK(child) <- m_OK(parents)
    parents <- .rm_value_if(parents, force_keep=force_keep)
    m_parents(child) <- list(parents)
  } else {
    if(inherit_value)
      m_value(child) <- lapply(parents, m_value)
    if(inherit_OK)
      m_OK(m) <- all(lapply(parents, m_OK))
    parents <- lapply(parents, .rm_value_if, force_keep=force_keep)
    m_parents(child) <- parents
  }
  child
}





#' @rdname rmonad_accessors
#' @export
m_parents  <- function(m) .maybe_vector_get(m@parents)

#' @rdname rmonad_accessors
#' @export
m_value    <- function(m) .maybe_vector_get(m@x)

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) m@OK

#' @rdname rmonad_accessors
#' @export
m_code     <- function(m) m@code

#' @rdname rmonad_accessors
#' @export
m_error   <- function(m) .maybe_vector_get(m@error)

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
`m_OK<-` <- function(m, value) {
  m@OK <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  m@x <- .maybe_vector_set(value, .not_empty)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  m@parents <- .maybe_vector_set(value, .not_empty)
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
  m@error <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  m@warnings <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  m@notes <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  m@doc <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
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
    m_warnings(m) <- append(value, m_warnings(m))
  }
  m
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value) {
  if(length(value) > 0 && nchar(value) > 0){
    m_notes(m) <- append(value, m_notes(m))
  }
  m
}

#' @rdname rmonad_accessors
#' @export
app_branch   <- function(m, value) {
  m_branch(m) <- append(value, m_branch(m))
  m
}
