#' Special rmonad_printerss
#'
#' @param x An object to print
#' @param ... Additional arguments going to God knows where
#' @name rmonad_printers


#' @rdname rmonad_printers
#' @export 
print.record <- function(x, ...) {
NULL

  if(length(x@code) > 0){
    cat(sprintf("R> %s\n", x@code)) 
  }
  if(length(x@errors) != 0){
    cat("Error: ")
    cat(paste(unlist(x@errors), collapse="\nError: "))
    cat("\n")
  }
  if(length(x@warnings) != 0){
    cat("Warning: ")
    cat(paste(unlist(x@warnings), collapse="\nWarning: "))
    cat("\n")
  }
  if(length(x@notes) != 0){
    cat("Note: ")
    cat(paste(unlist(x@notes), collapse="\nNote: "))
    cat("\n")
  }
  if(length(x@x) != 0){
    cat("Value:\n")
    print(x@x)
  }
}

#' @rdname rmonad_printers
#' @export 
print.rmonad <- function(x, ...){
  print(x@stage)

  if(length(x@x) == 1){
    cat("x: ")
    print(x@x[[1]])
  } else if(length(x@x > 1)) {
    print(x@x)
  }

  if(length(x@history) > 0){
    cat("\n ----------------- \n\n")
    f <- lapply(x@history, function(x) {print(x); cat("\n")})
  }
}
