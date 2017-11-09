#' Vectorized getters for public Rmonad fields
#'
#' @param m An Rmonad object
#' @param index Selection of indices to extract (all by default). The indices
#'              may be a vector of integers, node names, or igraph vertices
#'              (\code{igraph.vs}).
#' @param warn logical In get_value, raise a warning on an attempt to access an uncached node
#' @param ... Additional arguments
#' @name rmonad_getters
NULL

#' Vectorized existence checkers for public Rmonad fields
#'
#' @param m An Rmonad object
#' @param ... Additional arguments passed to \code{get_*} functions
#' @name rmonad_checkers
NULL


#' Determine wether something is an Rmonad object
#'
#' @param m Rmonad object
#' @return logical TRUE if m is an Rmonad
is_rmonad <- function(m) {
  setequal(class(m), "Rmonad")
}

# Delete a node's value
#
# @param m Rmonad object
.single_delete_value <- function(m) {
  .single_raw_value(m) <- no_cache()
  m
}

# The purpose of the following functions are to make the setting of things to
# blank (i.e. default, empty, or missing). Simply setting a value to NULL does
# not clearly express intent (are we deleting the value ro do we really want a
# NULL value?). Also there are multiple reasonable defaults (NULL, "", NA,
# NA_integer_, logical(0), etc) and use of the wrong one can be a source of
# subtle of reoccuring bugs. So I gather all this into one place.
.default_value      <- function() void_cache()
.default_head       <- function() 1L
.default_code       <- function() character(0)
.default_error      <- function() character(0)
.default_warnings   <- function() character(0)
.default_notes      <- function() character(0)
.default_OK         <- function() TRUE
.default_doc        <- function() character(0)
.default_mem        <- function() NA_real_
.default_time       <- function() NA_real_
.default_meta       <- function() list()
.default_nest_depth <- function() 1L
.default_stored     <- function() FALSE
.default_id         <- function() integer(0)
.default_summary    <- function() list()



# ======================== Vectorized existence checkers =======================

#' @rdname rmonad_checkers
#' @export
has_code <- function(m, ...) sapply(get_code(m, ...), .is_not_empty_string) %>% unname

#' @rdname rmonad_checkers
#' @export
has_error <- function(m, ...) sapply(get_error(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_doc <- function(m, ...) sapply(get_doc(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_warnings <- function(m, ...) sapply(get_warnings(m , ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_notes <- function(m, ...) sapply(get_notes(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_meta <- function(m, ...) sapply(get_meta(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_time <- function(m, ...) sapply(get_time(m, ...), .is_not_empty_real) %>% unname

#' @rdname rmonad_checkers
#' @export
has_mem <- function(m, ...) sapply(get_mem(m, ...), .is_not_empty_real) %>% unname

#' @rdname rmonad_checkers
#' @export
has_value <- function(m, ...) {
  sapply(
    .get_many_attributes(m, attribute='value', ...),
    function(x) {
      (class(x) == "CacheManager") && x@chk()
    }
  ) %>% unname
}

#' @rdname rmonad_checkers
#' @export
has_parents <- function(m, ...) sapply(get_parents(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_dependents <- function(m, ...) sapply(get_dependents(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_prior <- function(m, ...) sapply(get_prior(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_nest <- function(m, ...) sapply(get_nest(m, ...), function(x) length(x) > 0) %>% unname

#' @rdname rmonad_checkers
#' @export
has_summary <- function(m, ...) sapply(get_summary(m, ...), function(x) length(x) > 0) %>% unname



# ============================= Vectorized Getters =============================

#' @rdname rmonad_getters
#' @export
get_parents <- function(m, index=.get_ids(m)) {
  .get_many_relative_ids(
    m     = m,
    index = index,
    mode  = "in",
    type  = c("depend", "transitive")
  )
}

#' @rdname rmonad_getters
#' @export
get_dependents <- function(m, index=.get_ids(m)) {
  .get_many_relative_ids(m, index=index, mode="out", type="depend")
}

#' @rdname rmonad_getters
#' @export
get_nest <- function(m, index=.get_ids(m)) {
  .get_many_relative_ids(m, index=index, mode="in", type="nest")
}

#' @rdname rmonad_getters
#' @export
get_prior <- function(m, index=.get_ids(m)) {
  .get_many_relative_ids(m, index=index, mode="in", type="prior")
}

#' @rdname rmonad_getters
#' @export
get_nest_depth <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute='nest_depth') %>% as.integer
}

#' @rdname rmonad_getters
#' @export
get_value <- function(m, warn=TRUE, index=.get_ids(m)){
  lapply(.get_many_attributes(m, index=index, attribute='value'), function(v) v@get(warn))
}

#' @rdname rmonad_getters
#' @export
get_id <- function(m, index=.get_ids(m)) {
  # FIXME: should I use numeric or vertex ids?
  .get_numeric_ids(m, index=index) %>% as.integer
}

#' @rdname rmonad_getters
#' @export
get_OK <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="OK") %>% as.logical
}

#' @rdname rmonad_getters
#' @export
get_code <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute='code')
}

#' @rdname rmonad_getters
#' @export
get_error <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="error")
}

#' @rdname rmonad_getters
#' @export
get_warnings <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="warnings")
}

#' @rdname rmonad_getters
#' @export
get_notes <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="notes")
}

#' @rdname rmonad_getters
#' @export
get_doc <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="doc")
}

#' @rdname rmonad_getters
#' @export
get_meta <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute='meta')
}

#' @rdname rmonad_getters
#' @export
get_time <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="time") %>% as.numeric
}

#' @rdname rmonad_getters
#' @export
get_mem <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="mem") %>% as.numeric
}

#' @rdname rmonad_getters
#' @export
get_summary <- function(m, index=.get_ids(m)) {
  .get_many_attributes(m, index=index, attribute="summary")
}


# ============== Singular getters and setters (internal use only) ==============

.single_stored <- function(m, ...) {
  stored <- .get_single_attribute(m, attribute="stored", ...)
  if(is.null(stored)){
    FALSE
  } else {
    stored
  }
}
`.single_stored<-` <- function(m, value) {
  .set_single_attribute(m, attribute="stored", value=value)
}

.single_dependents <- function(m, ...) {
  .get_single_relative_ids(m, mode="out", type="depend", ...)
}
# no setter - see inherit

.single_prior <- function(m, ...) {
  .get_single_relative_ids(m, mode="in", type="prior", ...)
}
# no setter - see inherit

.single_id <- function(m, index=m@head) {
  .m_check(m)
  index
}
# no setter - automatically handled

.single_OK <- function(m, ...) {
  .get_single_attribute(m, attribute="OK", ...)
}
`.single_OK<-` <- function(m, value) {
  stopifnot(is.logical(value))
  .set_single_attribute(m, attribute="OK", value=value)
}

.single_value <- function(m, warn=TRUE, ...){
  .single_raw_value(m, ...)@get(warn=warn)
}
`.single_value<-` <- function(m, value) {
  .single_raw_value(m) <- memory_cache(value)
  m
}

.single_code <- function(m, ...) {
  .get_single_attribute(m, attribute="code", ...)
}
`.single_code<-` <- function(m, value) {
  .set_single_attribute(m, attribute="code", value=value)
}

.single_error <- function(m, ...) {
  .get_single_attribute(m, attribute="error", ...)
}
`.single_error<-` <- function(m, value) {
  .set_single_attribute(m, attribute="error", value=value)
}

.single_warnings <- function(m, ...) {
  .get_single_attribute(m, attribute="warnings", ...)
}
`.single_warnings<-` <- function(m, value) {
  .set_single_attribute(m, attribute="warnings", value=value)
}

.single_notes <- function(m, ...) {
  .get_single_attribute(m, attribute="notes", ...)
}
`.single_notes<-` <- function(m, value) {
  .set_single_attribute(m, attribute="notes", value=value)
}

.single_doc <- function(m, ...) {
  .get_single_attribute(m, attribute="doc", ...)
}
`.single_doc<-` <- function(m, value) {
  .set_single_attribute(m, attribute="doc", value=value)
}

.single_meta <- function(m, ...) {
  .get_single_attribute(m, attribute="meta", ...)
}
`.single_meta<-` <- function(m, value) {
  .set_single_attribute(m, attribute="meta", value=value)
}

.single_time <- function(m, ...) {
  .get_single_attribute(m, attribute="time", ...)
}
`.single_time<-` <- function(m, value) {
  .set_single_attribute(m, attribute="time", value=value)
}

.single_mem <- function(m, ...) {
  .get_single_attribute(m, attribute="mem", ...)
}
`.single_mem<-` <- function(m, value) {
  .set_single_attribute(m, attribute="mem", value=value)
}

.single_summary <- function(m, ...) {
  .get_single_attribute(m, attribute="summary", ...)
}
`.single_summary<-` <- function(m, value){
  .set_single_attribute(m, attribute="summary", value=value)
}

.single_parents <- function(m, ...) {
  .get_single_relative_ids(m, mode="in", type=c("depend", "transitive"), ...)
}
`.single_parents<-` <- function(m, value) {
  .add_parents(m, value, check=has_parents, type="depend")
}

.single_nest <- function(m, ...) {
  .get_single_relative_ids(m, mode="in", type="nest", ...)
}
`.single_nest<-` <- function(m, value) {
  if(.single_OK(value)){
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
    .single_raw_value(m) <- void_cache()
    m
  }
}

.single_nest_depth <- function(m, ...) {
  .get_single_attribute(m, attribute="nest_depth", ...)
}
`.single_nest_depth<-` <- function(m, value) {
  .set_single_attribute(m, attribute="nest_depth", value=value)
}
