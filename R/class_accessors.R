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
#' This is not the same as setting the value to NULL. Deleting tells
#' \code{rmonad} that no value is cached. If the value is accessed later, a
#' warning is raised. In contrast, setting the value to NULL will result in
#' \code{rmonad} thinking that the result of the computation was NULL.
#'
#' @export
#' @param m Rmonad object
m_delete_value <- function(m) {
  # TODO: uncomment next line (nothing bad could possibly happen if you do)
  # .m_check(m)
  m$delete_value()
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
  igraph::get.vertex.attribute(m@graph, attribute, m@head)
}

.setHeadAttribute <- function(m, attribute, value){
  .m_check(m)
  igraph::set.vertex.attribute(m@graph, attribute, m@head, value)
}

# TODO: export these?
has_code     = function(m) .is_not_empty_string(.getHeadAttribute(m, "code"))
has_error    = function(m) length(.getHeadAttribute(m, "error"))    != 0
has_doc      = function(m) length(.getHeadAttribute(m, "doc"))      != 0
has_warnings = function(m) length(.getHeadAttribute(m, "warnings")) != 0
has_notes    = function(m) length(.getHeadAttribute(m, "notes"))    != 0
has_meta     = function(m) length(.getHeadAttribute(m, "meta"))     != 0
has_time     = function(m) .is_not_empty_real(.getHeadAttribute(m, "time"))
has_mem      = function(m) .is_not_empty_real(.getHeadAttribute(m, "mem"))
has_value    = function(m) .getHeadAttribute(m, "value")(check=TRUE)
has_parents  = function(m) is.null(m_parents(m))
has_prior    = function(m) is.null(m_prior(m))
has_nest     = function(m) is.null(m_nest(m))
has_branch   = function(m) is.null(m_branch(m))

# TODO: chop these
.m_stored <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "stored")
}
`.m_stored<-` <- function(m, value) { m$set_stored(value) ; m }


.get_relative_ids <- function(m, mode, type){
  # FIXME: Directly using vertex ids is not a good idea; they are not stable in
  # general. In my particular case, I think they will be stable, but this is
  # dangerous. An alternative approach would be to add a unique name to each
  # node (e.g. a UUID) and using the names in head instead.
  vertices <- igraph::neighbors(m@graph, m@head, mode=mode) %>% as.numeric %>% unique

  edges <- igraph::incident_edges(m@graph, m@head, mode=mode)[[1]] %>% as.numeric

  stopifnot(length(vertices) == length(edges))
  
  etype <- igraph::get.edge.attribute(m@graph, "type", edges)

  stopifnot(length(etype) == length(edges))

  vertices[etype == type]

  if(length(vertices) == 0){
    # parent of root. FIXME: this is a bit arbitrary
    m <- NULL
  } else {
    m@head <- vertices
  }

  m
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
m_nest <- function(m) {
  .m_check(m)
  .get_relative_ids(m, "in", "nest")
}

#' @rdname rmonad_accessors
#' @export
m_prior <- function(m) {
  .m_check(m)
  .get_relative_ids(m, "in", "prior")
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
  .getHeadAttribute(m, "value")(...)
}

#' @rdname rmonad_accessors
#' @export
m_id <- function(m) {
  .m_check(m)
  m@head
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "value")(check=TRUE)
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "code")
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "error")
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "warnings")
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "notes")
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "doc")
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "meta")
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "time")
}


#' @rdname rmonad_accessors
#' @export
m_mem <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "mem")
}

#' @rdname rmonad_accessors
#' @export
m_summary <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "summary")
}

#' @rdname rmonad_accessors
#' @export
m_branch <- function(m) {
  .m_check(m)
  .getHeadAttribute(m, "branch")
}


#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "OK", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  .m_check(m)
  # TODO: Don't hardcode the cache function
  m <- .setHeadAttribute(m, "value", memoryCache(value))
  m
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "code", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "error", value)
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "warnings", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "notes", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "doc", value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .m_check(m)
  m <- .setHeadAttribute(m, "meta", value)
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
  m <- .setHeadAttribute(m, "summary", value)
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
  stopifnot(is.list(parents))
  for(p in parents){
    .m_check(p)
    child@graph <- p@graph + child@graph
    child@head <- vcount(p@graph) + child@head
    new_edge <- edge(p@head, child@head, ...)
    child@graph <- child@graph + new_edge
  }
  child
}

#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  .add_parents(m, value, check=has_parents, type="depend")
}

#' @rdname rmonad_accessors
#' @export
`m_nest<-` <- function(m, value) {
  .add_parents(m, value, check=has_nest, type="nest")
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
`m_branch<-` <- function(m, value) {
  .add_parents(m, value, check=has_branch, type="branch")
}

#' @rdname rmonad_accessors
#' @export
app_branch <- function(m, value) {
  .add_parents(m, value, check=false, type="branch")
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .add_parents(m, value, check=false, type="parents")
}
