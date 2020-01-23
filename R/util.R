# join two vectors
`%++%` <- function(l, r) { append(l, r) }

.is_not_empty_string = function(x) {
  is.character(x) &&
  !is.null(x)     &&
  !all(is.na(x))  &&
  (
    length(x) > 1 ||
    (length(x) == 1 && nchar(x) > 0)
  )
}

.is_not_empty_real = function(x) {
  !is.null(x) && !all(is.na(x)) && is.numeric(x) && length(x) != 0
}

.check_type <- function(
  m,
  type,
  test   = function(x) { setequal(class(x), type) },
  nframe = sys.nframe()-1,
  place  = if(nframe > 0) { deparse(sys.calls()[[nframe]]) } else { 'global' }
){
  if(!test(m)){
    varname <- deparse(substitute(m)) # NOTE: this has to be outside of glue
    stop(glue::glue(
      "In 'Rmonad::{place}', expected '{name}' to be of class {exp_type} but got '{obs_type}'",
      obs_type = class(m),
      name     = varname,
      place    = place,
      exp_type = type
    ))
  }
}
.m_check <- function(m, ...) {
  .check_type(m, test=is_rmonad, type='Rmonad', nframe=sys.nframe()-1, ...)
}

# NOTE: This is an internal function, used only in testing.
rmonad_equal <- function(a, b){
  size(a) == size(b) &&
  identical(get_value(a, warn=F), get_value(b, warn=F)) &&
  # -5 to remove the 'time' column, which generally won't be
  # the same between between rmonad runs
  identical(mtabulate(a, code=T)[, -5], mtabulate(b, code=T)[, -5]) &&
  identical(missues(a), missues(b))
}

.get_nest_salt <- function(){
  dynGet(".rmonad_nest_salt", ifnotfound=NULL, inherits=TRUE)
}

.set_nest_salt <- function(...){
  env <- parent.frame()
  assign(".rmonad_nest_salt", c(.get_nest_salt(), ...), envir=env)  
}

.parse_tags <- function(...){
  tags <- unlist(list(...))
  tags <- ifelse(tags == "", "/", tags)
  tags <- unlist(strsplit(tags, '/'))
  list(tag=tags, str=paste(tags, collapse='/'))
}

# Determine if a is prefixed by elements of b
#
# @param a vector or list
# @param b vector or list
# @return logical vector with length equal to the length of a
.a_has_prefix_b <- function(a, b){
  if(!is.list(a)){
    a <- list(a)
  }
  if(!is.list(b)){
    b <- list(b)
  }
  sapply(a, function(a_i){
    any(sapply(b, function(b_j){
      if(length(a_i) >= length(b_j)){
        identical(a_i[1:length(b_j)], b_j)
      } else {
        FALSE
      }
    }))
  })
}


# where
#  m := Rmonad object
#  tag := expected tag as character vector
# return indices
.match_tag <- function(m, tag, sep="/", by_prefix=FALSE){
  if(!is.null(tag)){
    tag <- unlist(strsplit(tag, sep))
  }
  if(by_prefix){
    which(sapply(get_tag(m),
           function(obs) any(sapply(obs, .a_has_prefix_b, tag))))
  } else {
    which(sapply(get_tag(m),
           function(obs) any(sapply(obs, identical, tag))))
  }
}

# filter function with normal R argument order
filter <- function(x, f){
  Filter(f, x)
}

# where
#  m := Rmonad object
#  tag := expected tag as character vector
# return ([name], [index])
.named_match_tag <- function(m, tag, sep="/"){
  if(!is.null(tag)){
    tag <- unlist(strsplit(tag, sep))
  }
  indices <- .match_tag(m, tag, by_prefix=TRUE)
  names <- lapply(
    get_tag(m, indices),
    filter,
    function(x){
      .a_has_prefix_b(x, tag) 
    }
  ) %>%
    lapply(head, 1) %>%
    lapply(lapply, function(x) paste0(x, collapse=sep)) %>%
    unlist
  list(indices=indices, names=names)
}
