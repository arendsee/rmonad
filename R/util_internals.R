# TODO: delete or repurpose this file

# Internal functions for manipulating Rmonad objects

# join two vectors
`%++%` <- function(l, r) { append(l, r) }

# add an element to a vector
# TODO: git rid of this, it conflicts with ggplot
`%+%` <- function(l, r) { l[[length(l)+1]] <- r; l }

.not_empty <- function(x) length(x) > 0
