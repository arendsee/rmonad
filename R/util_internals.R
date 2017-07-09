# Internal functions for manipulating Rmonad objects

.m_stored_value <- function(m) m@stage@x[[1]]

`.m_stored_value<-` <- function(m, value) { m@stage@x <- list(value) ; m }

# store a value
.store <- function(m){
  m@stage@x <- m@x 
  m
}


# Returns a history item from m, which is appriate for inclusion in the
# Rmonad@history list
.make_history <- function(m) {
  m_history(m) %+% m@stage
}

# store a value
.store_value <- function(m) { .m_stored_value(m) <- m_value(m) ; m }

# join two vectors
`%++%` <- function(l, r) { append(l, r) }

# add an element to a vector
`%+%` <- function(l, r) { l[[length(l)+1]] <- r; l }

# extra functional defaults
.false <- function(...) { FALSE }
.true  <- function(...) { TRUE  }

.has_value <- function(m) !is.null(m_value(m))

.not_empty <- function(x) length(x) > 0

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

.has_id    <- function(m) m_id(m) > 0

.has_code     <- function(m) .is_valid_string(m_code(m))

.has_error    <- function(m) length(m_error(m))    != 0
.has_doc      <- function(m) length(m_doc(m))      != 0
.has_warnings <- function(m) length(m_warnings(m)) != 0
.has_notes    <- function(m) length(m_notes(m))    != 0

.has_history  <- function(m) length(m_history(m))  != 0
.has_branch   <- function(m) length(m_branch(m))   != 0

.has_time     <- function(m) .is_valid_real(m_time(m))
.has_mem      <- function(m) .is_valid_integer(m_mem(m))
