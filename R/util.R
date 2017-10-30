# join two vectors
`%++%` <- function(l, r) { append(l, r) }

.is_not_empty_string = function(x) {
  !is.null(x)     &&
  !is.na(x)       &&
  is.character(x) &&
  (
    length(x) > 1 ||
    (length(x) == 1 && nchar(x) > 0)
  )
}

.is_not_empty_real = function(x) {
  !is.null(x) && !is.na(x) && is.numeric(x) && length(x) != 0
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
