#' Get, set, and append Rmonad fields
#'
#' @param m The Rmonad
#' @param value Value to replace or append current value
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
m_value    <- function(m) m@x[[1]]

#' @rdname rmonad_accessors
#' @export
m_code     <- function(m) m@stage@code

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
`m_history<-`  <- function(m, value) { m@history <- list(value); m }

#' @rdname rmonad_accessors
#' @export
`m_OK<-`       <- function(m, value) { m@OK <- value ; m }

#' @rdname rmonad_accessors
#' @export
`m_value<-`    <- function(m, value) { m@x <- list(value) ; m }

#' @rdname rmonad_accessors
#' @export
`m_code<-`     <- function(m, value) { m@stage@code <- value ; m }

#' @rdname rmonad_accessors
#' @export
`m_errors<-`   <- function(m, value) { m@stage@errors <- value ; }

#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) { m@stage@warnings <- list(value) ; }

#' @rdname rmonad_accessors
#' @export
`m_notes<-`    <- function(m, value) { m@stage@notes <- list(value) ; }

#' @rdname rmonad_accessors
#' @export
`m_doc<-`      <- function(m, value) { m@stage@doc <- value ; }

#' @rdname rmonad_accessors
#' @export
`m_branch<-`   <- function(m, value) { m@stage@branch <- list(value) ; }



#' @rdname rmonad_accessors
#' @export
app_history  <- function(m, value) {
  append(append(value@history, value@stage), m@history)
}

#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value) append(value, m@stage@warnings)

#' @rdname rmonad_accessors
#' @export
app_notes    <- function(m, value) append(value, m@stage@notes)

#' @rdname rmonad_accessors
#' @export
app_branch   <- function(m, value) append(value, m@stage@branch)
