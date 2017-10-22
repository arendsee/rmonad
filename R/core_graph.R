inherit <- function(
  child,
  parents,
  type          = "depend",
  inherit_value = FALSE,
  inherit_OK    = FALSE,
  force_keep    = FALSE
) {

  # to allow either list or single input
  if(is_rmonad(parents))
    parents <- list(parents)

  values <- list()

  for(g in parents){
    if(inherit_value){
      values <- append(values, igraph::get.vertex.attribute(g@graph, "value", g@head))
    }
    if(inherit_OK && !m_OK(g@graph)){
      child <- m_OK(child, FALSE)
    }
    if(!force_keep && !.m_stored(g)){
      g <- m_delete_value(g)
      .m_stored(g) <- FALSE
    } else {
      .m_stored(g) <- TRUE
    }
    child@graph <- g@graph + child@graph
    child@head <- child@head + igraph::vcount(g@graph)
    child@graph <- child@graph + igraph::edge(g@head, child@head, type=type)
  }

  # FIXME: let it be or not be not may be
  child_value <- if(length(values) == 1) {
    values[[1]]
  } else {
    values
  }

  if(inherit_value){
    child <- igraph::set.vertex.attribute(
      graph = child@graph,
      name  = "value",
      index = child@head,
      value = child_value
    )
  }
  child
}
