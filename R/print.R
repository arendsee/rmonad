#' Special rmonad_printerss
#'
#' @param x An object to print
#' @param ... Additional arguments going to God knows where
#' @name rmonad_printers

NULL

#' @rdname rmonad_printers
#' @export 
print.record <- function(x, ...) {
  if(length(x@code) > 0){
    cat(sprintf("R> %s\n", x@code)) 
  }
  if(nchar(x@doc) > 0){
    cat(sprintf("\n    %s\n\n", x@doc)) 
  }
  if(nchar(x@errors) != 0){
    cat(" * ERROR:", x@errors)
    cat("\n")
  }
  if(length(x@warnings) != 0){
    cat(" * WARNING: ")
    cat(paste(unlist(x@warnings), collapse="\n * WARNING: "))
    cat("\n")
  }
  if(length(x@notes) != 0){
    cat(" * NOTE: ")
    cat(paste(unlist(x@notes), collapse="\n * NOTE: "))
    cat("\n")
  }
  if(length(x@branch) != 0){
    cat("Has", length(x@branch), "branches\n")
  }
  if(length(x@x) == 1){
    cat("Value:\n")
    print(x@x[[1]])
  } else if(length(x@x > 1)) {
    print(x@x)
  }
}
setMethod("show", "record",
  function(object) print(object)
)

#' @rdname rmonad_printers
#' @export 
print.Rmonad <- function(x, ...){

  if(length(x@history) > 0){
    f <- lapply(x@history, function(x) {print(x); cat("\n")})
  }

  print(x@stage)

  if(length(x@history) > 0){
    cat("\n ----------------- \n\n")
  }

  if(length(x@x) == 1){
    cat("Value: ")
    print(x@x[[1]])
  } else if(length(x@x) > 1) {
    print(x@x)
  }

  if(!x@OK){
    cat("\n *** FAILURE *** \n")
  }
}
setMethod("show", "Rmonad",
  function(object) print(object)
)
