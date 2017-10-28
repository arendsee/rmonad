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
m_delete_value <- function(m, index=m@head) {
  caches <- .get_raw_value(m, index)
  for(cache in caches){
    cache@del()
  }
  m <- .set_raw_value(m, list(noCache()), index)
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

has_code     <- function(m, index=m@head) sapply(ms_code(m), .is_not_empty_string)[index]
has_error    <- function(m, index=m@head) sapply(ms_error(m),    function(x) length(x) > 0)[index]
has_doc      <- function(m, index=m@head) sapply(ms_doc(m),      function(x) length(x) > 0)[index]
has_warnings <- function(m, index=m@head) sapply(ms_warnings(m), function(x) length(x) > 0)[index]
has_notes    <- function(m, index=m@head) sapply(ms_notes(m),    function(x) length(x) > 0)[index]
has_meta     <- function(m, index=m@head) sapply(ms_meta(m),     function(x) length(x) > 0)[index]
has_time     <- function(m, index=m@head) sapply(ms_time(m), .is_not_empty_real)[index]
has_mem      <- function(m, index=m@head) sapply(ms_mem(m), .is_not_empty_real)[index]
has_value    <- function(m, index=m@head) sapply(.get_raw_value(m, ms_id(m)), function(x) x@chk())
has_parents    <- function(m, index=m@head) sapply(ms_parents(m),    function(x) length(x) > 0)[index]
has_dependents <- function(m, index=m@head) sapply(ms_dependents(m), function(x) length(x) > 0)[index]
has_prior      <- function(m, index=m@head) sapply(ms_prior(m),      function(x) length(x) > 0)[index]
has_nest       <- function(m, index=m@head) sapply(ms_nest(m),       function(x) length(x) > 0)[index]


# TODO: chop these
# FIXME: seriously, murder the stored field
.m_stored <- function(m, index=m@head) {
  stored <- .get_attribute_complex(m, "stored", index=index)
  if(is.null(stored)){
    FALSE
  } else {
    stored
  }
}
`.m_stored<-` <- function(m, value) {
  .set_attribute(m, "stored", value)
}

#' @rdname rmonad_accessors
#' @export
m_parents <- function(m, index=m@head) {
  .get_relative_ids(m, "in", c("depend", "transitive"), index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_parents <- function(m) {
  lapply(ms_id(m), function(i)
    .get_relative_ids(
      m     = m,
      mode  = "in",
      type  = c("depend", "transitive"),
      index = i
    ))
}

#' @rdname rmonad_accessors
#' @export
m_dependents <- function(m, index=m@head) {
  .get_relative_ids(m, "out", "depend", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_dependents <- function(m) {
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
  .get_attribute_complex(m, "nest_depth", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_nest_depth <- function(m) {
  .get_all_attribute(m, 'nest_depth')
}

#' @rdname rmonad_accessors
#' @export
m_value <- function(m, index=m@head, ...){
  # ... should only ever be 'warn' at this point
  .get_attribute_complex(m, "value", index=index)@get(...)
}

#' @rdname rmonad_accessors
#' @export
ms_value <- function(m, ...){
  .m_check(m)
  lapply(.get_all_attribute(m, 'value'), function(v) v@get(...))
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
  .get_numeric_ids(m)
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m, index=m@head) {
  .get_attribute_complex(m, "OK", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_OK <- function(m) {
  .get_all_attribute(m, "OK")
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m, index=m@head) {
  .get_attribute_complex(m, "code", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_code <- function(m) {
  .get_all_attribute(m, 'code')
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m, index=m@head) {
  .get_attribute_complex(m, "error", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_error <- function(m) {
  .get_all_attribute_complex(m, "error", default=NA_character_)
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m, index=m@head) {
  .get_attribute_complex(m, "warnings", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_warnings <- function(m) {
  .get_all_attribute_complex(m, "warnings", default=NA_character_)
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m, index=m@head) {
  .get_attribute_complex(m, "notes", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_notes <- function(m) {
  .get_all_attribute_complex(m, "notes", default=NA_character_)
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m, index=m@head) {
  .get_attribute_complex(m, "doc", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_doc <- function(m) {
  .get_all_attribute_complex(m, "doc", default=NA_character_)
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m, index=m@head) {
  .get_attribute_complex(m, "meta", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_meta <- function(m) {
  .get_all_attribute(m, 'meta')
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m, index=m@head) {
  .get_attribute_complex(m, "time", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_time <- function(m) {
  .get_all_attribute_complex(m, "time", default=NA_real_)
}

#' @rdname rmonad_accessors
#' @export
m_mem <- function(m, index=m@head) {
  .get_attribute_complex(m, "mem", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_mem <- function(m) {
  .get_all_attribute_complex(m, "mem", default=NA_integer_)
}

#' @rdname rmonad_accessors
#' @export
m_summary <- function(m, index=m@head) {
  .get_attribute_complex(m, "summary", index=index)
}

#' @rdname rmonad_accessors
#' @export
ms_summary <- function(m) {
  .get_all_attribute_complex(m, "summary", default=.default_summary())
}

#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  stopifnot(is.logical(value))
  .set_attribute(m, "OK", value)
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  # TODO: Don't hardcode the cache function
  .set_attribute_complex(m, "value", memoryCache(value))
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .set_attribute_complex(m, "code", value)
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .set_attribute_complex(m, "error", value)
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .set_attribute_complex(m, "warnings", value)
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .set_attribute_complex(m, "notes", value)
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .set_attribute_complex(m, "doc", value)
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .set_attribute_complex(m, "meta", value)
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  .set_attribute(m, "time", value)
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  .set_attribute(m, "mem", value)
}

#' @rdname rmonad_accessors
#' @export
`m_summary<-` <- function(m, value){
  .set_attribute_complex(m, "summary", value)
}


#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value, index=m@head) {
  warnings <- .get_attribute_complex(m, "warnings", index=index)
  if(length(value) > 0 && nchar(value) > 0){
    warnings <- value %++% warnings
  }
  .set_attribute(m, "warnings", warnings, index=index)
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value, index=m@head) {
  notes <- .get_attribute_complex(m, "notes", index=index)
  if(length(value) > 0 && nchar(value) > 0){
    notes <- value %++% notes
  }
  .set_attribute(m, "notes", notes, index=index)
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
  .set_attribute(m, "nest_depth", value)
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .add_parents(m, value, check=false, type="parents")
}
