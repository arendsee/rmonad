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

#' Delete a node's value
#'
#' @param m Rmonad object
#' @param index Delete the value contained by this vertex (if NULL, delete head value)
#' @export
m_delete_value <- function(m, ...) {
  caches <- .get_raw_value(m, ...)
  for(cache in caches){
    cache@del()
  }
  m <- .set_raw_value(m, list(noCache()), ...)
  m
}

# The purpose of the following functions are to make the setting of things to
# blank (i.e. default, empty, or missing). Simply setting a value to NULL does
# not clearly express intent (are we deleting the value ro do we really want a
# NULL value?). Also there are multiple reasonable defaults (NULL, "", NA,
# NA_integer_, logical(0), etc) and use of the wrong one can be a source of
# subtle of reoccuring bugs. So I gather all this into one place.
.default_value      <- function() NULL
.default_head       <- function() 1L
.default_code       <- function() character(0)
.default_error      <- function() character(0)
.default_warnings   <- function() character(0)
.default_notes      <- function() character(0)
.default_OK         <- function() TRUE
.default_doc        <- function() character(0)
.default_mem        <- function() NA
.default_time       <- function() NA
.default_meta       <- function() NULL
.default_nest_depth <- function() 1
.default_stored     <- function() FALSE
.default_id         <- function() integer(0)
.default_summary    <- function() NULL

has_code       <- function(m, ...) sapply(ms_code(m              , ...), .is_not_empty_string      )
has_error      <- function(m, ...) sapply(ms_error(m             , ...), function(x) length(x) > 0 )
has_doc        <- function(m, ...) sapply(ms_doc(m               , ...), function(x) length(x) > 0 )
has_warnings   <- function(m, ...) sapply(ms_warnings(m          , ...), function(x) length(x) > 0 )
has_notes      <- function(m, ...) sapply(ms_notes(m             , ...), function(x) length(x) > 0 )
has_meta       <- function(m, ...) sapply(ms_meta(m              , ...), function(x) length(x) > 0 )
has_time       <- function(m, ...) sapply(ms_time(m              , ...), .is_not_empty_real        )
has_mem        <- function(m, ...) sapply(ms_mem(m               , ...), .is_not_empty_real        )
has_value      <- function(m, ...) sapply(.get_many_raw_values(m , ...), function(x) x@chk()       )
has_parents    <- function(m, ...) sapply(ms_parents(m           , ...), function(x) length(x) > 0 )
has_dependents <- function(m, ...) sapply(ms_dependents(m        , ...), function(x) length(x) > 0 )
has_prior      <- function(m, ...) sapply(ms_prior(m             , ...), function(x) length(x) > 0 )
has_nest       <- function(m, ...) sapply(ms_nest(m              , ...), function(x) length(x) > 0 )

# TODO: chop these
# FIXME: seriously, murder the stored field
.m_stored <- function(m, ...) {
  stored <- .get_single_attribute_complex(m, default=FALSE, attribute="stored", ...)
  if(is.null(stored)){
    FALSE
  } else {
    stored
  }
}
`.m_stored<-` <- function(m, value) {
  .set_single_attribute(m, attribute="stored", value=value)
}

#' @rdname rmonad_accessors
#' @export
m_parents <- function(m, ...) {
  .get_single_relative_ids(m, "in", c("depend", "transitive"), ...)
}

#' @rdname rmonad_accessors
#' @export
ms_parents <- function(m, ...) {
  .get_many_relative_ids(
    m     = m,
    mode  = "in",
    type  = c("depend", "transitive"),
    ...
  )
}

#' @rdname rmonad_accessors
#' @export
m_dependents <- function(m, ...) {
  .get_single_relative_ids(m, "out", "depend", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_dependents <- function(m, ...) {
  .get_many_relative_ids(m=m, mode="out", type="depend", ...)
}

#' @rdname rmonad_accessors
#' @export
m_nest <- function(m, ...) {
  .get_single_relative_ids(m, "in", "nest", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_nest <- function(m, ...) {
  .get_many_relative_ids(m=m, mode="in", type="nest", ...)
}

#' @rdname rmonad_accessors
#' @export
m_prior <- function(m, ...) {
  .get_single_relative_ids(m, "in", "prior", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_prior <- function(m, ...) {
  .get_many_relative_ids(m=m, mode="in", type="prior", ...)
}

#' @rdname rmonad_accessors
#' @export
m_nest_depth <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_nest_depth(), attribute="nest_depth", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_nest_depth <- function(m, ...) {
  .get_many_attributes(m, attribute='nest_depth', ...)
}

#' @rdname rmonad_accessors
#' @export
m_value <- function(m, warn=TRUE, ...){
  # ... should only ever be 'warn' at this point
  .get_single_attribute_complex(m, default=.default_value(), attribute="value", ...)@get(warn)
}

#' @rdname rmonad_accessors
#' @export
ms_value <- function(m, warn=TRUE, ...){
  lapply(.get_many_attributes(m, attribute='value', ...), function(v) v@get(warn))
}

#' @rdname rmonad_accessors
#' @export
m_id <- function(m, index=m@head) {
  .m_check(m)
  index
}

#' @rdname rmonad_accessors
#' @export
ms_id <- function(m, ...) {
  .get_numeric_ids(m, ...)
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_OK(), attribute="OK", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_OK <- function(m, ...) {
  .get_many_attributes(m, attribute="OK", ...)
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_code(), attribute="code", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_code <- function(m, ...) {
  .get_many_attributes(m, attribute='code', ...)
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_error(), attribute="error", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_error <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="error", default=NA_character_, ...)
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_warnings(), attribute="warnings", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_warnings <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="warnings", default=NA_character_, ...)
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_notes(), attribute="notes", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_notes <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="notes", default=NA_character_, ...)
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_doc(), attribute="doc", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_doc <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="doc", default=NA_character_, ...)
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_meta(), attribute="meta", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_meta <- function(m, ...) {
  .get_many_attributes(m, attribute='meta', ...)
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_time(), attribute="time", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_time <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="time", default=NA_real_, ...)
}

#' @rdname rmonad_accessors
#' @export
m_mem <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_mem(), attribute="mem", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_mem <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="mem", default=NA_integer_, ...)
}

#' @rdname rmonad_accessors
#' @export
m_summary <- function(m, ...) {
  .get_single_attribute_complex(m, default=.default_summary(), attribute="summary", ...)
}

#' @rdname rmonad_accessors
#' @export
ms_summary <- function(m, ...) {
  .get_many_attributes_complex(m, attribute="summary", default=.default_summary(), ...)
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  stopifnot(is.logical(value))
  .set_single_attribute(m, attribute="OK", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  # TODO: Don't hardcode the cache function
  .set_single_attribute_complex(m, attribute="value", value=memoryCache(value))
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .set_single_attribute_complex(m, attribute="code", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .set_single_attribute_complex(m, attribute="error", value=value)
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .set_single_attribute_complex(m, attribute="warnings", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .set_single_attribute_complex(m, attribute="notes", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .set_single_attribute_complex(m, attribute="doc", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .set_single_attribute_complex(m, attribute="meta", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  .set_single_attribute(m, attribute="time", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  .set_single_attribute(m, attribute="mem", value=value)
}

#' @rdname rmonad_accessors
#' @export
`m_summary<-` <- function(m, value){
  .set_single_attribute_complex(m, attribute="summary", value=value)
}


#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value, ...) {
  warnings <- .get_single_attribute_complex(m, attribute="warnings", ...)
  if(length(value) > 0 && nchar(value) > 0){
    warnings <- value %++% warnings
  }
  .set_single_attribute(m, attribute="warnings", warnings, ...)
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value, ...) {
  notes <- .get_single_attribute_complex(m, attribute="notes", ...)
  if(length(value) > 0 && nchar(value) > 0){
    notes <- value %++% notes
  }
  .set_single_attribute(m, attribute="notes", notes, ...)
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
  .set_single_attribute(m, attribute="nest_depth", value=value)
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .add_parents(m, value, check=false, type="parents")
}
