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
# `unlist(get_parents(m))`
.get_edge_types <- function(m){
  .m_check(m)
  igraph::E(m@graph)$type
}

# Handle linking of child node to a single parent node
#
# @param child          Rmonad object
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
    .single_raw_value(child) <- .single_raw_value(parent)

  if(inherit_OK && !.single_OK(parent))
    .single_OK(child) <- .single_OK(parent)

  if(!force_keep && !.single_stored(parent)){
    parent <- .single_delete_value(parent)
    .single_stored(parent) <- FALSE
  } else {
    .single_stored(parent) <- TRUE
  }

  child@graph <- .rmonad_union(parent@graph, child@graph)
  child@graph <- child@graph  + igraph::edge(parent@head, child@head, type=type)
  # FIXME: need to resolve any possible name conflicts here
  child@data <- append(parent@data, child@data)

  child
}


# The following functions resolve conflicts in attributes that arise with the
# union of two igraph objects. If any attributes are shared between the graphs,
# then they are copied into new vectors with the prefixes `_1` and `_2` added.
# -----------------------------------------------------------------------------
# This function does NOT create an edge between the two graphs, it only merges
# them into one and handles attributes.
#
# FIXME: Also, there is little real detection or handling of conflicts here.
# This ought to holler if nodes with the same uuid have conflicting values, but
# instead I just grab the value from the parent.
.rmonad_union <- function(a, b){
  ab <- igraph::union(a, b, byname=TRUE)
  ab <- .zip_edge(ab)
  ab
}
.zip_edge <- function(ab){
  xs <- igraph::get.edge.attribute(ab, "type_1")
  ys <- igraph::get.edge.attribute(ab, "type_2")
  if(is.null(xs)){
    return(ab)
  }
  if(any(xs != ys, na.rm=TRUE)){
    stop("Edge type conflict, either you were being naughty or there is a bug in Rmonad")
  }
  if(any(is.na(xs) & is.na(ys))){
    stop("Untyped edge, either you were being naughty or there is a bug in Rmonad")
  }
  ab <- igraph::set.edge.attribute(ab, 'type', value=ifelse(is.na(xs), ys, xs))
  ab <- igraph::delete_edge_attr(ab, "type_1")
  ab <- igraph::delete_edge_attr(ab, "type_2")
  ab
}

# If an Rmonad holds an Rmonad value, link the value as a nest parent 
.unnest <- function(m){
  if(is_rmonad(m) && has_value(m, index=m@head) && is_rmonad(.single_value(m))){
    nest <- .single_value(m)
    m <- .set_many_attributes(
      m,
      attribute='nest_depth',
      value = .get_many_attributes(m, attribute='nest_depth') +
              (.single_nest_depth(m) - .single_nest_depth(nest) + 1)
    )
    .single_nest(m) <- nest 
  }
  m
}

# Make an empty, directed graph
.new_rmonad_graph <- function(m){
  node_id <- uuid::UUIDgenerate()
  m@graph <- igraph::make_empty_graph(directed=TRUE, n=1)
  m@graph <- igraph::set.vertex.attribute(m@graph, "name", value=node_id)
  m@head <- node_id
  m
}

.get_ids <- function(m, index=NULL){
  .m_check(m)
  ids <- igraph::V(m@graph)
  if(!is.null(index)){
    ids <- ids[index]
  }
  ids
}
.get_numeric_ids <- function(...){
  .get_ids(...) %>% as.numeric
}

.as_index <- function(m, index){
  if(is.numeric(index)){
    index <- igraph::get.vertex.attribute(m@graph, name='name', index=index)
  }
  index
}

# Add multiple parents to an child 
#
# @param child Rmonad object
# @param parents Rmonad object or list of Rmonad objects
# @param check function UNNECESSARY?
.add_parents <- function(child, parents, check=false, ...){
  .m_check(child)
  stopifnot(!check(child))
  if(!is.list(parents)){
    parents <- list(parents)
  }
  for(p in parents){
    .m_check(p)
    child@graph <- .rmonad_union(p@graph, child@graph)
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
.get_single_relative_ids <- function(m, mode, type, index=m@head, as_integer=TRUE){
  .m_check(m)
  vertices <- igraph::neighbors(m@graph, index, mode=mode)
  edges <- igraph::incident_edges(m@graph, index, mode=mode)[[1]]
  stopifnot(length(vertices) == length(edges))
  etype <- igraph::get.edge.attribute(m@graph, "type", edges)
  stopifnot(length(etype) == length(edges))
  parents <- vertices[etype %in% type]
  if(as_integer){
    as.integer(parents)
  } else { 
    parents
  }
}
.get_many_relative_ids <- function(m, index=.get_ids(m), ...){
  lapply(index, function(i) .get_single_relative_ids(m, index=i, ...)) %>% unname
}

# Get attributes for specified indicies
#
# @param m Rmonad object
# @param attribute The attribute name (e.g. "error")
# @param index vector of indices
.get_attribute <- function(m, attribute, index=m@head){
  .m_check(m)
  lapply(m@data[.as_index(m, index)], slot, attribute)
}
.get_many_attributes <- function(m, index=.get_ids(m), ...){
  .get_attribute(m, index=index, ...) %>% unname
}
.get_single_attribute <- function(m, index=m@head, ...){
  if(length(index) != 1){
    stop(".single_* accessors only take a single index, to get multiple values, use the get_* accessors")
  }
  .get_attribute(m, index=index, ...)[[1]]
}

# Set attributes at specified indices
#
# This works for simple values where `length(index)` == `length(value)`
#
# @param m Rmonad object
# @param attribute The attribute name (e.g. "error")
# @param value attribute value
# @param index vector of indices
.set_single_attribute <- function(m, attribute, value, index=m@head){
  .m_check(m)
  if(length(index) != 1){
    stop("ERROR: Can only set one attribute at a time in .single_* setters")
  }
  slot(m@data[[.as_index(m, index)]], attribute) <- value
  m
}
.set_many_attributes <- function(m, attribute, value, index=.get_ids(m), ...){
  m@data[.as_index(m, index)] <- lapply(
    m@data[.as_index(m, index)],
    function(x) { x[[attribute]] <- value }
  ) 
  m
}

.single_raw_value <- function(m, ...){
  .get_single_attribute(m, attribute = 'value', ...)
}
`.single_raw_value<-` <- function(m, value) {
  .set_single_attribute(m, attribute="value", value=value)
}
