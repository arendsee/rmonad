#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @name rmonad_accessors

NULL

.maybe_vector_get <- function(x){
  if(length(x) == 0){
    NULL   # Nothing
  } else {
    x[[1]] # a
  }
}

.maybe_vector_set <- function(x, is_not_empty, expected_type=.true){
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
m_history  <- function(m) m@history

#' @rdname rmonad_accessors
#' @export
m_value    <- function(m) .maybe_vector_get(m@x)

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) {
  if(class(m) == "Rmonad"){
    m@stage@OK
  } else {
    m@OK
  }
}

#' @rdname rmonad_accessors
#' @export
m_code     <- function(m) {
  if(class(m) == "Rmonad"){
    m@stage@code
  } else {
    m@code
  }
}

#' @rdname rmonad_accessors
#' @export
m_id       <- function(m) {
  if(class(m) == "Rmonad"){
    m@stage@id
  } else {
    m@id
  }
}

#' @rdname rmonad_accessors
#' @export
m_error   <- function(m) {
  x <- if(class(m) == "Rmonad"){
    m@stage@error
  } else {
    m@error
  }
  .maybe_vector_get(x)
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) {
  x <- if(class(m) == "Rmonad"){
    m@stage@warnings
  } else {
    m@warnings
  }
  .maybe_vector_get(x)
}

#' @rdname rmonad_accessors
#' @export
m_notes    <- function(m) {
  x <- if(class(m) == "Rmonad"){
    m@stage@notes
  } else {
    m@notes
  } 
  .maybe_vector_get(x)
}

#' @rdname rmonad_accessors
#' @export
m_doc      <- function(m) {
  x <- if(class(m) == "Rmonad"){
    m@stage@doc
  } else {
    m@doc
  }
  .maybe_vector_get(x)
}

#' @rdname rmonad_accessors
#' @export
m_time     <- function(m) {
  time <- if(class(m) == "Rmonad"){
    m@stage@other$time
  } else {
    m@other$time
  }
  if(is.null(time)){
    NA_real_
  } else {
    time
  }
}


#' @rdname rmonad_accessors
#' @export
m_mem      <- function(m) {
  mem <- if(class(m) == "Rmonad"){
    m@stage@other$mem
  } else {
    m@other$mem
  }
  if(is.null(mem)){
    NA_real_
  } else {
    mem 
  }
}

#' @rdname rmonad_accessors
#' @export
m_branch   <- function(m) {
  if(class(m) == "Rmonad"){
    m@stage@branch
  } else {
    m@branch
  }
}



#' @rdname rmonad_accessors
#' @export
`m_history<-`  <- function(m, value) {
  m@history <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  m@stage@OK <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_id<-` <- function(m, value) {
  m@stage@id <- value
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
`m_code<-` <- function(m, value) {
  m@stage@code <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  m@stage@error <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  m@stage@warnings <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  m@stage@notes <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  m@stage@doc <- .maybe_vector_set(value, .is_valid_string, expected_type=is.character)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  m@stage@other$time <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  m@stage@other$mem <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_branch<-` <- function(m, value) {
  m@stage@branch <- value
  m
}



#' @rdname rmonad_accessors
#' @export
app_history <- function(m, value) {
  m_history(m) <- append(m@history, append(value@stage, value@history))
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
