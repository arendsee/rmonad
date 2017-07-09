# TODO: delete or repurpose this file

# Internal functions for manipulating Rmonad objects

# Returns a history item from m, which is appropriate for inclusion in the
# Rmonad@history list
.make_history <- function(m) {
  m_history(m) %+% m@stage
}

# join two vectors
`%++%` <- function(l, r) { append(l, r) }

# add an element to a vector
# TODO: git rid of this, it conflicts with ggplot
`%+%` <- function(l, r) { l[[length(l)+1]] <- r; l }

.has_value <- function(m) !is.null(m_value(m))

.not_empty <- function(x) length(x) > 0
