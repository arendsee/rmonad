#' Special rmonad_printerss
#'
#' @param x An object to print
#' @param ... Additional arguments going to God knows where
#' @name rmonad_printers

NULL

# sugar!
.scat <- function(s, ...) cat(sprintf(s, ...)) 

#' @rdname rmonad_printers
#' @export 
print.record <- function(x, ...) {

  .scat("R> %s", m_code(x))

  if(.has_doc(x)){
    .scat("\n\n    %s\n\n", m_doc(x))
  }
  if(.has_error(x)){
    .scat("\n * ERROR: %s", m_error(x))
  }
  if(.has_warnings(x)){
    .scat("\n * WARNING: %s",
      paste(unlist(m_warnings(x)), collapse="\n * WARNING: ")
    )
  }
  if(.has_notes(x)){
    .scat("\n * NOTE: %s",
      paste(unlist(m_notes(x)), collapse="\n * NOTE: ")
    )
  }
  if(.has_branch(x)){
    .scat("\nHas %s branches", length(.has_branch(x)))
  }
  if(.has_value(x)){
    cat("\n")
    print(m_value(x))
  }
}
setMethod("show", "record",
  function(object) print(object)
)

#' @rdname rmonad_printers
#' @export 
print.Rmonad <- function(x, ...){

  if(.has_history(x)){
    f <- lapply(m_history(x), function(x) {print(x); cat("\n")})
  }

  print(x@stage)

  if(.has_history(x)){
    cat("\n\n ----------------- \n\n")
  }

  if(.has_value(x)){
    print(m_value(x))
  }

  if(!m_OK(x)){
    cat(" *** FAILURE *** \n")
  }
}
setMethod("show", "Rmonad",
  function(object) print(object)
)
