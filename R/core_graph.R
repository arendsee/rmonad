# These are internal functions, for connecting parents to children

.m_stored <- function(m) {
  m@.stored
}
`.m_stored<-` <- function(m, value) { m@.stored <- value ; m }

.rm_value_if <- function(m, force_keep=FALSE){
  if(!force_keep && !.m_stored(m)){
    m_value(m) <- NULL
    .m_stored(m) <- FALSE
  } else {
    .m_stored(m) <- TRUE
  }
  m
}

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
