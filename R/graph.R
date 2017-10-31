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
    child <- .set_raw_value(child, .get_raw_value(parent))

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

  child
}


# The following functions resolve conflicts in attributes that arise with the
# union of two igraph objects. If any attributes are shared between the graphs,
# then they are copied into new vectors with the prefixes `_1` and `_2` added.
# -----------------------------------------------------------------------------
# This function does NOT create an edge between the two graphs, it only merges
# them into one and handles attributes.
.rmonad_union <- function(a, b){
  ab <- igraph::union(a, b, byname=TRUE)
  ab <- .resolve_edge_attributes(ab)
  ab <- .resolve(ab, 'value',      .by_value)
  ab <- .resolve(ab, 'code',       .by_1)
  ab <- .resolve(ab, 'error',      .by_1)
  ab <- .resolve(ab, 'warnings',   .by_1)
  ab <- .resolve(ab, 'notes',      .by_1)
  ab <- .resolve(ab, 'OK',         .by_1)
  ab <- .resolve(ab, 'doc',        .by_1)
  ab <- .resolve(ab, 'mem',        .by_1)
  ab <- .resolve(ab, 'time',       .by_1)
  ab <- .resolve(ab, 'meta',       .by_1)
  ab <- .resolve(ab, 'nest_depth', .by_1)
  ab <- .resolve(ab, 'summary',    .by_1)
  ab <- .resolve(ab, 'stored',     .by_1)
  ab
}
.resolve_edge_attributes <- function(ab){
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
# Resolve field IF there is no conflict
# FIXME: this dies on overlapping graphs
.resolve <- function(ab, field, merger, ...){
  xs <- igraph::get.vertex.attribute(ab, paste0(field, "_1"))
  ys <- igraph::get.vertex.attribute(ab, paste0(field, "_2"))

  if(is.null(xs)){
    return(ab)
  }

  ab <- igraph::set.vertex.attribute(ab, field, value=merger(xs, ys, ...))
  ab <- igraph::delete_vertex_attr(ab, paste0(field, "_1"))
  ab <- igraph::delete_vertex_attr(ab, paste0(field, "_2"))

  ab
}
.by_1 <- function(xs, ys, ...){
  has_y <- !is.na(ys)
  has_x <- !is.na(xs)
  joint <- rep(NA, length(xs))
  joint[!(has_y | has_x)] <- NA
  joint[has_y] <- ys[has_y]
  joint[has_x] <- xs[has_x]
  joint
}
.by_or_die <- function(xs, ys, ...){
  if(all(xor(is.na(xs), is.na(ys)))){
    ifelse(is.na(xs), ys, xs)
  } else {
    stop("Rmonad error: cannot handle conflicts")
  }
}
.by_value <- function(xs, ys, ...){
  x_has_value <- sapply(xs, function(x) (class(x) == 'CacheManager') && x@chk())
  y_has_value <- sapply(ys, function(y) (class(y) == 'CacheManager') && y@chk())
  x_is_managed <- sapply(xs, function(x) class(x) == 'CacheManager')
  y_is_managed <- sapply(ys, function(y) class(y) == 'CacheManager')
  joint <- ifelse(y_has_value, ys, NA)
  joint <- ifelse(x_has_value, xs, joint)
  joint <- ifelse(y_is_managed, ys, joint)
  joint <- ifelse(x_is_managed, xs, joint)
  joint
}
# -----------------------------------------------------------------------------


# If an Rmonad holds an Rmonad value, link the value as a nest parent 
.unnest <- function(m){
  if(is_rmonad(m) && has_value(m, index=m@head) && is_rmonad(.single_value(m))){
    nest <- .single_value(m)
    nest@graph <- igraph::set.vertex.attribute(
      graph = nest@graph,
      name  = "nest_depth",
      value = igraph::V(nest@graph)$nest_depth + (.single_nest_depth(m) - .single_nest_depth(nest) + 1)
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
  igraph::get.vertex.attribute(m@graph, name=attribute, index=index)
}
.get_many_attributes <- function(m, index=.get_ids(m), ...){
  .get_attribute(m, index=index, ...) 
}
.get_single_attribute <- function(m, default, index=m@head, ...){
  if(length(index) != 1){
    stop(".single_* accessors only take a single index, to get multiple values, use the get_* accessors")
  }
  a <- .get_attribute(m, index=index, ...)
  if(is.null(a) || length(a) == 0){
    a <- default
  }
  a
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
  m@graph <- igraph::set.vertex.attribute(m@graph, name=attribute, index=index, value=value)
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
.get_single_attribute_complex <- function(...){
  a <- .get_single_attribute(...)
  if(is.null(a)){
    NULL # FIXME: should not assume a NULL as default
  } else {
    a[[1]]
  }
}
.get_many_attributes_complex <- function(m, default=NA, ...){
  .get_many_attributes(m, ...) %>% {
    if(is.null(.)){
      . <- rep(default, size(m))
    }
    .
  }
}
.set_single_attribute_complex <- function(m, value, ...){
  .set_single_attribute(m, value=list(value), ...)
}

# Set the value function
#
# The Rmonad `value` field is internally a function that returns the stored
# value. This function sets the cache function, not just the stored value.
#
# @param m Rmonad object
# @param value A cache function
# @param index vector of indices
.set_raw_value <- function(m, value, ...){
  .set_single_attribute_complex(m, attribute='value', value=value, ...)
}

# Get the value function
#
# @param m Rmonad object
# @param index vector of indices
.get_raw_value <- function(m, ...){
  .get_single_attribute_complex(m, default=.default_value(), attribute='value', ...)
}
.get_many_raw_values <- function(m, ...){
  .get_many_attributes_complex(m, attribute='value', default=.default_value(), ...)
}
