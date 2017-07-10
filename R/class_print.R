#' Special rmonad printers
#'
#' @param x An object to print
#' @param verbose logical print verbose output (include benchmarking)
#' @param ... Additional arguments going to God knows where
#' @name rmonad_printers

NULL

# sugar!
.scat <- function(s, ...) cat(sprintf(s, ...)) 

.print_record <- function(x, verbose=FALSE, print_value=TRUE, ...) {

  .scat('#%s> "%s"', m_id(x), paste(m_code(x), collapse="\n"))

  if(verbose && (.has_time(x) || .has_mem(x))){
    cat("\n  ")
    if(.has_mem(x))  { .scat(" size: %s", m_mem(x))  }
    if(.has_time(x)) { .scat(" time: %s", m_time(x)) }
  }

  if(.has_doc(x)){
    .scat("\n\n    %s\n\n", m_doc(x))
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

#' @rdname rmonad_printers
#' @export 
print.Rmonad <- function(x, verbose=FALSE, ...){

  ms <- monad_to_list(x)

  for(i in seq_len(length(ms)-1)){
    .print_record(ms[[i]], print_value=TRUE)
  }
  .print_record(x, print_value=FALSE)

  if(length(ms) > 1){
    cat("\n ----------------- \n\n")
  }

  print(m_value(x))

  if(!m_OK(x)){
    cat(" *** FAILURE *** \n")
  }
}
setMethod("show", "Rmonad",
  function(object) print(object)
)
