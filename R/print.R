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
  if(.has_code(x)){
    .scat("R> %s\n", m_code(x))
  }
  if(.has_doc(x)){
    .scat("\n    %s\n\n", m_doc(x))
  }
  if(.has_error(x)){
    .scat(" * ERROR: %s", m_error(x))
  }
  if(.has_warnings(x)){
    .scat(" * WARNING: %s\n",
      paste(unlist(m_warnings(x)), collapse="\n * WARNING: ")
    )
  }
  if(.has_notes(x)){
    .scat(" * NOTE: %s\n",
      paste(unlist(m_notes(x)), collapse="\n * NOTE: ")
    )
  }
  if(.has_branch(x)){
    .scat("Has %s branches\n", length(.has_branch(x)))
  }
  if(.has_value(x)){
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
    cat("\n ----------------- \n\n")
  }

  if(.has_value(x)){
    print(m_value(x))
  }

  if(!m_OK(x)){
    cat("\n *** FAILURE *** \n")
  }
}
setMethod("show", "Rmonad",
  function(object) print(object)
)
