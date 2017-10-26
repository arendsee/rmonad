# This file contains internal functions for interfacing with the igraph object
# wrapped by the Rmonad object. All direct calls to the igraph library should
# be in this file. Ideally, it should be possible to swap igraph for some other
# network library by changing only this code (and the plot function).

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

  if(inherit_value)
    child@graph <- igraph::set.vertex.attribute(
      graph = child@graph,
      name  = "value",
      index = child@head,
      value = igraph::get.vertex.attribute(parent@graph, "value", parent@head)
    )

  if(inherit_OK && !m_OK(parent))
    m_OK(child) <- m_OK(parent)

  if(!force_keep && !.m_stored(parent)){
    parent <- m_delete_value(parent)
    .m_stored(parent) <- FALSE
  } else {
    .m_stored(parent) <- TRUE
  }

  child@graph <- parent@graph + child@graph
  child@head  <- child@head   + igraph::vcount(parent@graph)
  child@graph <- child@graph  + igraph::edge(parent@head, child@head, type=type)

  child
}


.add_parents <- function(child, parents, check=false, ...){
  .m_check(child)
  stopifnot(!check(child))
  if(!is.list(parents)){
    parents <- list(parents)
  }
  for(p in parents){
    .m_check(p)
    child@graph <- p@graph + child@graph
    child@head <- igraph::vcount(p@graph) + child@head
    new_edge <- igraph::edge(p@head, child@head, ...)
    child@graph <- child@graph + new_edge
  }
  child
}


.get_relative_ids <- function(m, mode, type, index=m@head){
  # FIXME: Directly using vertex ids is not a good idea; they are not stable in
  # general. In my particular case, I think they will be stable, but this is
  # dangerous. An alternative approach would be to add a unique name to each
  # node (e.g. a UUID) and using the names in head instead.
  vertices <- igraph::neighbors(m@graph, index, mode=mode) %>% as.numeric
  edges <- igraph::incident_edges(m@graph, index, mode=mode)[[1]] %>% as.numeric
  stopifnot(length(vertices) == length(edges))
  etype <- igraph::get.edge.attribute(m@graph, "type", edges)
  stopifnot(length(etype) == length(edges))
  vertices[etype == type] %>% as.integer
}


.getAttribute <- function(m, attribute, index){
  .m_check(m)
  a <- igraph::get.vertex.attribute(m@graph, attribute, index)
  if(is.null(a)){
    a
  } else {
    a[[1]]
  }
}


.setAttribute <- function(m, attribute, value, index=m@head){
  .m_check(m)
  m@graph <- igraph::set.vertex.attribute(m@graph, attribute, index, value)
  m
}
