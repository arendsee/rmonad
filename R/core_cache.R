#' Represent a value that has been deleted
#'
#' By default, the value of a node that has already been executed will be set
#' to this function.
#'
#' @param warn logical Emit a warning on access 
noCache <- function(warn=TRUE){
  if(warn){
    warning("Attempting to access data that has been deleted, returning NULL")
  }
  NULL
}

#' Store a value in memory
#'
#' @param x Value to be stored
#' @examples
#' foo <- 45
#' foo_proxy <- memoryCache(foo)
#' foo
#' foo_proxy()
memoryCache <- function(x){
  function() x
}

#' Make a function of x that caches data locally
#'
#' @param path A directory in which to cache results
#' @examples
#' \dontrun{
#'   foo <- 45
#'   localCacher <- makeLocalCacher(".")
#'   foo_ <- localCacher(45)
#'   rm(foo)
#'   foo_()
#' }
makeLocalCacher <- function(path){
  # Save x and return a function that can load it
  function(x){
    filename <- file.path(path, paste0(uuid::UUIDgenerate(), ".Rdata"))
    if(!dir.exists(path)){
      dir.create(path, recursive=TRUE)
    }
    save(x, file=filename) 
    rm(x)
    function(){
      load(filename)
      x
    }
  }
}
