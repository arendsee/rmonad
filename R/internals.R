# Internal functions for manipulating Rmonad objects

.m_stored_value <- function(m) m@stage@x[[1]]

`.m_stored_value<-` <- function(m, value) { m@stage@x <- list(value) ; m }

# store a value
.store_value <- function(m) { .m_stored_value(m) <- m_value(m) ; m }

# join two vectors
`%++%` <- function(l, r) { append(l, r) }

# add an element to a vector
`%+%` <- function(l, r) { l[[length(l)+1]] <- r; l }
