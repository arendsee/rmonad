# TODO: delete or repurpose this file

# Internal functions for manipulating Rmonad objects

# join two vectors
`%++%` <- function(l, r) { append(l, r) }

# add an element to a vector
# TODO: git rid of this, it conflicts with ggplot
`%+%` <- function(l, r) { l[[length(l)+1]] <- r; l }

.not_empty <- function(x) length(x) > 0

# convert an rmonad object to a list of monads
monad_to_list <- function(m){
  ms <- list(m)
  for(b in m_branch(m)){
    ms <- append(monad_to_list(b), ms)
  }
  for(p in m_parents(m)){
    ms <- append(monad_to_list(p), ms)
  }
  unique(ms)
}
