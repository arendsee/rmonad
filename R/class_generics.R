.scat <- function(s, ...) cat(sprintf(s, ...)) 

.print_record <- function(x, verbose=FALSE, print_value=TRUE, ...) {

  if(.has_doc(x)){
    .scat("\n\n    %s\n\n", m_doc(x))
  }
  .scat('R> "%s"', paste(m_code(x), collapse="\n"))

  if(verbose && (.has_time(x) || .has_mem(x))){
    cat("\n  ")
    if(.has_mem(x))  { .scat(" size: %s", m_mem(x))  }
    if(.has_time(x)) { .scat(" time: %s", m_time(x)) }
  }
  if(.has_error(x)){
    .scat("\n * ERROR: %s", m_error(x))
  }
  if(.has_warnings(x)){
    .scat("\n * WARNING: %s",
      paste(m_warnings(x), collapse="\n * WARNING: ")
    )
  }
  if(.has_notes(x)){
    .scat("\n * NOTE: %s",
      paste(m_notes(x), collapse="\n * NOTE: ")
    )
  }
  if(.has_branch(x)){
    .scat("\nHas %s branches", length(.has_branch(x)))
  }
  if(.m_stored(x) && print_value){
    cat("\n")
    print(m_value(x))
  }
  cat("\n")
}

#' Rmonad print generic function
#'
#' @param x An Rmonad object
#' @param verbose logical print verbose output (include benchmarking)
#' @param print_value logical print the value wrapped in the Rmonad
#' @param ... Additional arguments (unused)
#' @export
print.Rmonad <- function(x, verbose=FALSE, print_value=TRUE, ...){

  ms <- as.list(x)

  for(i in seq_len(length(ms)-1)){
    .print_record(ms[[i]], print_value=print_value)
  }
  .print_record(x, print_value=FALSE)

  if(length(ms) > 1){
    cat("\n ----------------- \n\n")
  }

  if(print_value)
    print(m_value(x))

  if(!m_OK(x)){
    cat(" *** FAILURE *** \n")
  }
}
setMethod("show", "Rmonad",
  function(object) print(object)
)

#' Convert an rmonad object to a list of monads
#'
#' @param x An Rmonad object
#' @param ... Additional arguments (unused)
#' @export
as.list.Rmonad <- function(x, ...){
  ms <- list(x)
  for(b in m_branch(x)){
    ms <- as.list(b) %++% ms
  }
  for(p in m_parents(x)){
    ms <- as.list(p) %++% ms
  }
  unique(ms)
}
