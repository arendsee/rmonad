#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @param index The index of the node to get or set
#' @param ... Additional arguments
#' @name rmonad_accessors
NULL



#' Determine wether something is an Rmonad object
#'
#' @param m Rmonad object
#' @return logical TRUE if m is an Rmonad
is_rmonad <- function(m) {
  setequal(class(m), "Rmonad")
}

#' Return the number of nodes in the workflow
#'
#' @param m Rmonad object
size <- function(m) {
  .m_check(m)
  igraph::vcount(m@graph)
}

#' Delete a node's value
#'
#' @param m Rmonad object
#' @param index Delete the value contained by this vertex (if NULL, delete head value)
#' @export
m_delete_value <- function(m, index=m@head) {
  caches <- .get_raw_value(m, index)
  for(cache in caches){
    cache@del()
  }
  m <- .set_raw_value(m, list(noCache()), index)
  m
}

has_code     = function(m, index=m@head) sapply(ms_code(m), .is_not_empty_string)[index]
has_error    = function(m, index=m@head) sapply(ms_error(m),    function(x) length(x) > 0)[index]
has_doc      = function(m, index=m@head) sapply(ms_doc(m),      function(x) length(x) > 0)[index]
has_warnings = function(m, index=m@head) sapply(ms_warnings(m), function(x) length(x) > 0)[index]
has_notes    = function(m, index=m@head) sapply(ms_notes(m),    function(x) length(x) > 0)[index]
has_meta     = function(m, index=m@head) sapply(ms_meta(m),     function(x) length(x) > 0)[index]
has_time     = function(m, index=m@head) sapply(ms_time(m), .is_not_empty_real)[index]
has_mem      = function(m, index=m@head) sapply(ms_mem(m), .is_not_empty_real)[index]
has_value    = function(m, index=m@head) sapply(.get_raw_value(m, ms_id(m)), function(x) x@chk())
has_parents  = function(m, index=m@head) sapply(ms_parents(m),  function(x) length(x) > 0)[index]
has_children = function(m, index=m@head) sapply(ms_children(m), function(x) length(x) > 0)[index]
has_prior    = function(m, index=m@head) sapply(ms_prior(m),    function(x) length(x) > 0)[index]
has_nest     = function(m, index=m@head) sapply(ms_nest(m),     function(x) length(x) > 0)[index]


# TODO: chop these
# FIXME: seriously, murder the stored field
.m_stored <- function(m, index=m@head) {
  stored <- .getAttribute(m, "stored", index=index)
  if(is.null(stored)){
    FALSE
  } else {
    stored
  }
}
`.m_stored<-` <- function(m, value) {
  .setAttribute(m, "stored", value)
}

#' @rdname rmonad_accessors
#' @export
m_parents <- function(m, index=m@head) {
  .get_relative_ids(m, "in", "depend", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_parents <- function(m) {
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="in", type="depend", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_children <- function(m, index=m@head) {
  .get_relative_ids(m, "out", "depend", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_children <- function(m) {
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="out", type="depend", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_nest <- function(m, index=m@head) {
  .get_relative_ids(m, "in", "nest", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_nest <- function(m) {
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="in", type="nest", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_prior <- function(m, index=m@head) {
  .get_relative_ids(m, "in", "prior", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_prior <- function(m) {
  lapply(ms_id(m), function(i) .get_relative_ids(m=m, mode="in", type="prior", index=i))
}

#' @rdname rmonad_accessors
#' @export
m_nest_depth <- function(m, index=m@head) {
  .getAttribute(m, "nest_depth", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_nest_depth <- function(m) {
  igraph::V(m@graph)$nest_depth
}

#' @rdname rmonad_accessors
#' @export
m_value <- function(m, index=m@head, ...){
  # ... should only ever be 'warn' at this point
  .getAttribute(m, "value", index=index)@get(...)
}

#' @rdname rmonad_accessors
#' @export
ms_value <- function(m, ...){
  .m_check(m)
  lapply(igraph::V(m@graph)$value, function(v) v@get(...))
}

#' @rdname rmonad_accessors
#' @export
m_id <- function(m, index=m@head) {
  .m_check(m)
  index
}

#' @rdname rmonad_accessors
#' @export
ms_id <- function(m) {
  .m_check(m)
  igraph::V(m@graph) %>% as.numeric
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m, index=m@head) {
  .getAttribute(m, "OK", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_OK <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$OK
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m, index=m@head) {
  .getAttribute(m, "code", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_code <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$code
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m, index=m@head) {
  .getAttribute(m, "error", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_error <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$error %>% {
    if(is.null(.)){
      . <- rep(NA_character_, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m, index=m@head) {
  .getAttribute(m, "warnings", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_warnings <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$warnings %>% {
    if(is.null(.)){
      . <- rep(NA_character_, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m, index=m@head) {
  .getAttribute(m, "notes", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_notes <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$notes %>% {
    if(is.null(.)){
      . <- rep(NA_character_, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m, index=m@head) {
  .getAttribute(m, "doc", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_doc <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$doc %>% {
    if(is.null(.)){
      . <- rep(NA_character_, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m, index=m@head) {
  .getAttribute(m, "meta", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_meta <- function(m) {
  .m_check(m)
  igraph::get.vertex.attribute(m@graph, "meta")
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m, index=m@head) {
  .getAttribute(m, "time", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_time <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$time %>% {
    if(is.null(.)){
      . <- rep(NA_real_, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_mem <- function(m, index=m@head) {
  .getAttribute(m, "mem", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_mem <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$mem %>% {
    if(is.null(.)){
      . <- rep(NA_integer_, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
m_summary <- function(m, index=m@head) {
  .getAttribute(m, "summary", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_summary <- function(m) {
  .m_check(m)
  igraph::V(m@graph)$summary %>% {
    if(is.null(.)){
      . <- rep(NA, size(m))
    }
    .
  }
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  stopifnot(is.logical(value))
  .setAttribute(m, "OK", value)
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  # TODO: Don't hardcode the cache function
  .setAttribute(m, "value", list(memoryCache(value)))
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .setAttribute(m, "code", list(value))
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .setAttribute(m, "error", list(value))
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .setAttribute(m, "warnings", list(value))
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .setAttribute(m, "notes", list(value))
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .setAttribute(m, "doc", list(value))
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .setAttribute(m, "meta", list(value))
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  .setAttribute(m, "time", value)
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  .setAttribute(m, "mem", value)
}

#' @rdname rmonad_accessors
#' @export
`m_summary<-` <- function(m, value){
  .setAttribute(m, "summary", list(value))
}


#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value, index=m@head) {
  warnings <- .getAttribute(m, "warnings", index=index)
  if(length(value) > 0 && nchar(value) > 0){
    warnings <- value %++% warnings
  }
  .setAttribute(m, "warnings", warnings, index=index)
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value, index=m@head) {
  notes <- .getAttribute(m, "notes", index=index)
  if(length(value) > 0 && nchar(value) > 0){
    notes <- value %++% notes
  }
  .setAttribute(m, "notes", notes, index=index)
}



#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  .add_parents(m, value, check=has_parents, type="depend")
}

#' @rdname rmonad_accessors
#' @export
`m_nest<-` <- function(m, value) {
  if(m_OK(value)){
    .inherit(
      child         = m,
      parent        = value,
      inherit_value = TRUE,
      inherit_OK    = TRUE,
      force_keep    = FALSE,
      type          = "nest"
    )
  } else {
    m <- .inherit(
      child         = m,
      parent        = value,
      inherit_value = FALSE,
      inherit_OK    = TRUE,
      force_keep    = TRUE,
      type          = "nest"
    )
    m <- .set_raw_value(m, voidCache())
    m
  }
}

#' @rdname rmonad_accessors
#' @export
`m_nest_depth<-` <- function(m, value) {
  .setAttribute(m, "nest_depth", value)
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .add_parents(m, value, check=false, type="parents")
}
