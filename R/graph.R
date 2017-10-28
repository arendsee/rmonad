# This file contains mostly internal functions for interfacing with the igraph
# object wrapped by the Rmonad object. All direct calls to the igraph library
# should be in this file. Ideally, it should be possible to swap igraph for
# some other network library by changing only this code (and the plot
# function).

#' Return the number of nodes in the workflow
#'
#' @param m Rmonad object
size <- function(m) {
  .m_check(m)
  igraph::vcount(m@graph)
}

# Add an edge between two nodes
.connect <- function(m, from, to, type='depend'){
  m@graph <- m@graph + igraph::edge(from, to, type=type)
  m
}

# These edges should be in the same order as vertex parents in
# `unlist(ms_parents(m))`
.get_edge_types <- function(m){
  .m_check(m)
  igraph::E(m@graph)$type
}

# Handle linking of child node to a single parent node
#
# @param child R        monad object
# @param parent         Rmonad object
# @param type           Edge type ["depend", "nest", "prior", "transitive"]
# @param inherit_value  logical should value be passed (on success)?
# @param inherit_OK     logical should status be passed?
# @param force_keep     logical should the parent value be cached?
.inherit <- function(
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

# If an Rmonad holds an Rmonad value, link the value as a nest parent 
.unnest <- function(m){
  if(is_rmonad(m) && has_value(m) && is_rmonad(m_value(m))){
    nest <- m_value(m)
    nest@graph <- igraph::set.vertex.attribute(
      graph = nest@graph,
      name  = "nest_depth",
      value = igraph::V(nest@graph)$nest_depth + (m_nest_depth(m) - m_nest_depth(nest) + 1)
    )
    m_nest(m) <- nest 
  }
  m
}

# Make an empty, directed graph
.new_rmonad_graph <- function(){
  igraph::make_empty_graph(directed=TRUE, n=1)
}

# Add multiple parents to an child 
#
# @param child Rmonad object
# @param parents Rmonad object or list of Rmonad objects
# @param check function UNNECESSARY?
.add_parents <- function(child, parents, check=false, ...){
  # FIXME: `check` is not being used, I removed it for a reason ... 
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

# Get the ids of neighbors connected by a given edge type
#
# @param m Rmonad object
# @param mode "in" or "out"
# @param type Edge types to consider
# @param index vector of indices
.get_relative_ids <- function(m, mode, type, index=m@head){
  .m_check(m)
  vertices <- igraph::neighbors(m@graph, index, mode=mode) %>% as.numeric
  edges <- igraph::incident_edges(m@graph, index, mode=mode)[[1]] %>% as.numeric
  stopifnot(length(vertices) == length(edges))
  etype <- igraph::get.edge.attribute(m@graph, "type", edges)
  stopifnot(length(etype) == length(edges))
  vertices[etype %in% type] %>% as.integer
}

.get_ids <- function(m){
  .m_check(m)
  igraph::V(m@graph)
}
.get_numeric_ids <- function(m){
  .m_check(m)
  igraph::V(m@graph) %>% as.numeric
}

# Get attributes for specified indicies
#
# @param m Rmonad object
# @param attribute The attribute name (e.g. "error")
# @param index vector of indices
.get_attribute <- function(m, attribute, index=m@head){
  .m_check(m)
  igraph::get.vertex.attribute(m@graph, attribute, index)
}
.get_all_attribute <- function(m, ...){
  .get_attribute(m, index=ms_id(m), ...) 
}

# Set attributes at specified indices
#
# This works for simple values where `length(index)` == `length(value)`
#
# @param m Rmonad object
# @param attribute The attribute name (e.g. "error")
# @param value attribute value
# @param index vector of indices
.set_attribute <- function(m, attribute, value, index=m@head){
  .m_check(m)
  m@graph <- igraph::set.vertex.attribute(m@graph, attribute, index, value)
  m
}


# Get attributes
#
# This is not the appropriate function for every attribute. Currently the
# implementation is a bit hacky. But some things have to be stored as lists in
# the igraph object. If the attribute is not NULL, we return the first element
# in the list. These lists should always be of length 1.
#
# @param ... Arguments passed to .get_attribute
.get_attribute_complex <- function(...){
  a <- .get_attribute(...)
  if(is.null(a)){
    NULL
  } else {
    a[[1]]
  }
}
.get_all_attribute_complex <- function(m, default=NA, ...){
  .get_all_attribute(m, ...) %>% {
    if(is.null(.)){
      . <- rep(default, size(m))
    }
    .
  }
}
.set_attribute_complex <- function(m, attribute, value, ...){
  .set_attribute(m, attribute, list(value))
}

# Set the value function
#
# The Rmonad `value` field is internally a function that returns the stored
# value. This function sets the cache function, not just the stored value.
#
# @param m Rmonad object
# @param value A cache function
# @param index vector of indices
.set_raw_value <- function(m, value, index=m@head){
  m@graph <- igraph::set.vertex.attribute(m@graph, "value", index=index, value=value)
  m
}

# Get the value function
#
# @param m Rmonad object
# @param index vector of indices
.get_raw_value <- function(m, index=m@head){
  .m_check(m)
  igraph::get.vertex.attribute(m@graph, "value", index=index)
}
