#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @name rmonad_accessors

NULL



#' @rdname rmonad_accessors
#' @export
m_history  <- function(m) m@history

#' @rdname rmonad_accessors
#' @export
m_OK       <- function(m) m@OK

#' @rdname rmonad_accessors
#' @export
m_value    <- function(m) if(length(m@x) > 0) { m@x[[1]] } else { NULL }

#' @rdname rmonad_accessors
#' @export
m_code     <- function(m) {
  m@code %||% m@stage@code %>% esc
}

#' @rdname rmonad_accessors
#' @export
m_errors   <- function(m) m@stage@errors

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) m@stage@warnings

#' @rdname rmonad_accessors
#' @export
m_notes    <- function(m) m@stage@notes

#' @rdname rmonad_accessors
#' @export
m_doc      <- function(m) m@stage@doc

#' @rdname rmonad_accessors
#' @export
m_branch   <- function(m) m@stage@branch



#' @rdname rmonad_accessors
#' @export
`m_history<-`  <- function(m, value) {
  m@history <- value
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
  m@x <- list(value)
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
`m_errors<-` <- function(m, value) {
  m@stage@errors <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  m@stage@warnings <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  m@stage@notes <- value
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  m@stage@doc <- value
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
