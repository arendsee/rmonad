#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @param warn Warn if the accessed field does not exist (value was not cached)
#' @name rmonad_accessors
NULL



#' Determine wether something is an Rmonad object
#'
#' @param m Rmonad object
#' @return logical TRUE if m is an Rmonad
is_rmonad <- function(m) {
  setequal(class(m), "Rmonad")
}

# internal utility for generating error messages when accessing a non-Rmonad
.m_check <- function(m) {
  if(!is_rmonad(m)){
    msg="Expected an Rmonad object, got %s"
    stop(sprintf(msg, class(m)))
  }
}

#' Delete a node's value
#'
#' @param m Rmonad object
#' @param index Delete the value contained by this vertex (if NULL, delete head value)
#' @export
m_delete_value <- function(m, index=NULL) {
  .m_check(m)
  if(is.null(index)){
    index <- m@head
  }
  caches <- igraph::get.vertex.attribute(m@graph, "value", index)
  for(cache in caches){
    cache@del()
  }
  m@graph <- igraph::set.vertex.attribute(m@graph, "value", index, list(noCache()))
  m
}


.is_not_empty = function(x) length(x) > 0

.is_not_empty_string = function(x) {
  !is.null(x)     &&
  !is.na(x)       &&
  is.character(x) &&
  (
    length(x) > 1 ||
    (length(x) == 1 && nchar(x) > 0)
  )
}

.is_not_empty_integer = function(x) {
  !is.null(x) && !is.na(x) && is.integer(x) && length(x) != 0
}

.is_not_empty_real = function(x) {
  !is.null(x) && !is.na(x) && is.numeric(x) && length(x) != 0
}

# === A note about Maybe ======================================================
# Some of the values stored in Rmonad could reasonably contain nothing, which
# is not the same as NULL. The value a node wraps be anything. But intermediate
# values are not usually stored (unless using %v>% or relatives), so we need a
# way to distinguish between a node holding no value and NULL. My approach is
# to emulate the Haskell Maybe by using a list of length 0 or 1. An empty list
# is Nothing. A list with one element, is Something.
.maybe_vector_get = function(x){
  if(length(x) == 0){
    NULL   # Nothing
           # NOTE: this is still ambiguous
  } else {
    x$value # a
  }
}

.maybe_vector_set = function(x, is_not_empty, expected_type=true){
  if(is_not_empty(x)){
    if(!expected_type(x)){
      stop("Type error")
    }
    list(value=x) # Just a
  } else {
    list()  # Nothing
  }
}

.getHeadAttribute <- function(m, attribute){
  .m_check(m)
  a <- igraph::get.vertex.attribute(m@graph, attribute, m@head)
  if(is.null(a)){
    a
  } else {
    a[[1]]
  }
}

.setHeadAttribute <- function(m, attribute, value){
  .m_check(m)
  m@graph <- igraph::set.vertex.attribute(m@graph, attribute, m@head, value)
  m
}

# TODO: export these?
has_code     = function(m) .is_not_empty_string(m_code(m))
has_error    = function(m) length(m_error(m))    > 0
has_doc      = function(m) length(m_doc(m))      > 0
has_warnings = function(m) length(m_warnings(m)) > 0
has_notes    = function(m) length(m_notes(m))    > 0
has_meta     = function(m) length(m_meta(m))     > 0
has_time     = function(m) .is_not_empty_real(.getHeadAttribute(m, "time"))
has_mem      = function(m) .is_not_empty_real(.getHeadAttribute(m, "mem"))
has_value    = function(m) .getHeadAttribute(m, "value")@chk()
has_parents  = function(m) length(m_parents(m))  > 0
has_children = function(m) length(m_children(m)) > 0
has_prior    = function(m) length(m_prior(m))    > 0
has_nest     = function(m) length(m_nest(m))     > 0

# TODO: chop these
# FIXME: seriously, murder the stored field
.m_stored <- function(m) {
  .m_check(m)
  stored <- .getHeadAttribute(m, "stored")
  if(is.null(stored)){
    FALSE
  } else {
    stored
  }
}
`.m_stored<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "stored", value)
  m
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

# TODO: I should be able to remove most of these functions, replace them with
# generic attribute getters and setters. This would reduce code repitition.
# However, it would also slightly complicate mapping these functions over
# rmonads. For example, common tasks like `lapply(x, m_value)` would become
# `lapply(x, function(y) get_attr(y, "value"))`. I am sure there is a clean
# solution, but for now, during this refactor, I want to keep changes to a
# minimum.

#' @rdname rmonad_accessors
#' @export
m_parents <- function(m) {
  .m_check(m)
  .get_relative_ids(m, "in", "depend")
}

#' @rdname rmonad_accessors
#' @export
ms_parents <- function(m) {
  .m_check(m)
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="in", type="depend", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_children <- function(m) {
  .m_check(m)
  .get_relative_ids(m, "out", "depend")
}

#' @rdname rmonad_accessors
#' @export
ms_children <- function(m) {
  .m_check(m)
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="out", type="depend", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_nest <- function(m) {
  .m_check(m)
  .get_relative_ids(m, "in", "nest")
}

#' @rdname rmonad_accessors
#' @export
ms_nest <- function(m) {
  .m_check(m)
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="in", type="nest", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_prior <- function(m) {
  .m_check(m)
  .get_relative_ids(m, "in", "prior")
}

#' @rdname rmonad_accessors
#' @export
ms_prior <- function(m) {
  .m_check(m)
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="in", type="prior", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_nest_depth <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "nest_depth")
}

#' @rdname rmonad_accessors
#' @export
m_value <- function(m, ...){
  .m_check(m)
  # ... should only ever be 'warn' at this point
  .getHeadAttribute(m, "value")@get(...)
}

#' @rdname rmonad_accessors
#' @export
ms_value <- function(m, ...){
  .m_check(m)
  lapply(igraph::V(m@graph)$value, function(v) v@get(...))
}

#' @rdname rmonad_accessors
#' @export
m_id <- function(m) {
  .m_check(m)
  m@head
}

#' @rdname rmonad_accessors
#' @export
ms_id <- function(m) {
  .m_check(m)
  igraph::V(m@graph) %>% as.numeric
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "OK")
}

#' @rdname rmonad_accessors
#' @export
ms_OK <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$OK
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "code")
}

#' @rdname rmonad_accessors
#' @export
ms_code <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$code
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "error")
}

#' @rdname rmonad_accessors
#' @export
ms_error <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$error %>% {
    if(is.null(.)){
      . <- rep(NA_character_, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "warnings")
}

#' @rdname rmonad_accessors
#' @export
ms_warnings <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$warnings %>% {
    if(is.null(.)){
      . <- rep(NA_character_, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "notes")
}

#' @rdname rmonad_accessors
#' @export
ms_notes <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$notes %>% {
    if(is.null(.)){
      . <- rep(NA_character_, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "doc")
}

#' @rdname rmonad_accessors
#' @export
ms_doc <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$doc %>% {
    if(is.null(.)){
      . <- rep(NA_character_, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$meta %>% {
    if(is.null(.)){
      . <- rep(NA, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "time")
}

#' @rdname rmonad_accessors
#' @export
ms_time <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$time %>% {
    if(is.null(.)){
      . <- rep(NA_real_, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_mem <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "mem")
}

#' @rdname rmonad_accessors
#' @export
ms_mem <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$mem %>% {
    if(is.null(.)){
      . <- rep(NA_integer_, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_summary <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "summary")
}

#' @rdname rmonad_accessors
#' @export
ms_summary <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$summary %>% {
    if(is.null(.)){
      . <- rep(NA, igraph::vcount(m@graph))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  stopifnot(is.logical(value))
  .m_check(m)
  m <- .setHeadAttribute(m, "OK", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  .m_check(m)
  # TODO: Don't hardcode the cache function
  m <- .setHeadAttribute(m, "value", list(memoryCache(value)))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "code", list(value))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "error", list(value))
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "warnings", list(value))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "notes", list(value))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "doc", list(value))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "meta", list(value))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "time", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "mem", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_summary<-` <- function(m, value){
  .m_check(m)
  m <- .setHeadAttribute(m, "summary", list(value))
  m
}


#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value) {
  .m_check(m)
  warnings <- .getHeadAttr(m, "warnings")
  if(length(value) > 0 && nchar(value) > 0){
    warnings <- value %++% warnings
  }
  .setHeadAttr(m, "warnings", warnings)
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value) {
  .m_check(m)
  notes <- .getHeadAttr(m, "notes")
  if(length(value) > 0 && nchar(value) > 0){
    notes <- value %++% notes
  }
  .setHeadAttr(m, "notes", notes)
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

#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  .m_check(m)
  .add_parents(m, value, check=has_parents, type="depend")
}

#' @rdname rmonad_accessors
#' @export
`m_nest<-` <- function(m, value) {
  .m_check(m)
  .m_check(value)
  inherit(child=m, parent=value, inherit_value=TRUE, inherit_OK=TRUE, type="nest")
}

#' @rdname rmonad_accessors
#' @export
`m_nest_depth<-` <- function(m, value) {
  .m_check(m)
  stop("NOT IMPLEMENTED")
  m
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .m_check(m)
  .add_parents(m, value, check=false, type="parents")
}
