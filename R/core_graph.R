inherit <- function(
  child,
  parent,
  type          = "depend",
  inherit_value = FALSE,
  inherit_OK    = FALSE,
  force_keep    = FALSE
) {

  if(!is_rmonad(parent))
    stop("In 'inherit', parent must be an Rmonad object")

  if(!is_rmonad(child))
    stop("In 'inherit', child must be an Rmonad object")

  if(inherit_value){
    value <- igraph::get.vertex.attribute(parent@graph, "value", parent@head)
  }
  if(inherit_OK && !m_OK(parent)){
    m_OK(m) <- m_OK(parent)
  }
  if(!force_keep && !.m_stored(parent)){
    parent <- m_delete_value(parent)
    .m_stored(parent) <- FALSE
  } else {
    .m_stored(parent) <- TRUE
  }
  child@graph <- parent@graph + child@graph
  child@head <- child@head + igraph::vcount(parent@graph)
  child@graph <- child@graph + igraph::edge(parent@head, child@head, type=type)

  if(inherit_value){
    child@graph <- igraph::set.vertex.attribute(
      graph = child@graph,
      name  = "value",
      index = child@head,
      value = value
    )
  }
  child
}
