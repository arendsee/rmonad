pass <- function(x) {
  list(x=x, errors=list(), warnings=list(), notes=list(), is_valid=TRUE) 
}

fail <- function(s) {
  list(x=NULL, errors=list(s), warnings=list(), notes=list(), is_valid=FALSE) 
}

warn <- function(m, s) {
  if(! .is_error_monad(m)){
    m <- pass(m)
  }
  m$warnings <- append(m$warnings, s)
  m
}

note <- function(m, s) {
  if(! .is_error_monad(m)){
    m <- pass(m)
  }
  m$notes <- append(m$notes, s)
  m
}

#' This function is appropriate when function f will not fail
#'
#' @param m Either a monadic error container or a simple value
#' @param f A function to apply to the value contained by the container
fmap <- function(m, f){
  if(.is_error_monad(m)){
    # Apply function only if m is in the passing state
    # Otherwise propagate the failure
    if(m$is_valid){
      m$x <- f(m$x)
    }
  } else {
    # If m is not an error container, then just apply f to it 
    m <- f(m)
  }
  m
}

bind <- function(xs, f){
  # load inputs in error container if the are not already in one
  ms <- lapply(.load_x, xs)

  if(all(lapply(ms, function(x) x$is_valid)))
  {
    # merge notes and warnings, replace value
    y <- do.call(f, x)
    y$notes    <- append(x$notes, y$notes)
    y$warnings <- append(x$warnings, y$warnings)
  }
  else
  {
    # propagate error
    y <- x
  }
  y
}

.is_error_monad <- function(x){
  is.list(x) &&
    length(names(x) == 5) &&
    all (names(x) == c("x", "errors", "warnings", "notes", "is_valid")) 
}

.load_x <- function(x){
  if (.is_error_moad(x)) { x } else { pass(x) }
}

.get_values <- function(ms){
  if(all(lapply(ms, function(m) m$is_valid))){
    lapply(ms, function(m) m$x)
  } else {
    Reduce(.propagate, ms, pass(NULL)) 
  }
}

.propagate <- function(m1, m2){
  m <- list(x        = NULL,
            errors   = append(m1$errors, m2$errors),
            notes    = append(m1$notes, m2$notes),
            warnings = append(m1$warnings, m2$warnings),
            is_valid = FALSE
           )
  if(m1$is_valid && m2$is_valid){
    m$x <- append(m1$x, m2$x)
    m$is_valid <- TRUE
  }
  m
}
