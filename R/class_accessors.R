#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @name rmonad_accessors
NULL

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
