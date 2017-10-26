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

# internal utility for generating error messages when accessing a non-Rmonad
.m_check <- function(m) {
  if(!is_rmonad(m)){
    msg="Expected an Rmonad object, got %s"
    stop(sprintf(msg, class(m)))
  }
}

