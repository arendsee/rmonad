#' Represent a value that has not been set
#'
#' This is the default value of RmonadNode@value. It should always be replaced
#' shortly after the object is created, thus should only be encountered if 1)
#' the user is directly creating RmonadNode objects (in which case they should
#' be spoken to sternly) or 2) there is a bug in rmonad.
#'
#' @return NULL
voidCache <- function(){
  # @param warn Warn if the accessed field does not exist (value was not cached)
  get <- function(warn=TRUE){
    if(warn){
      warning("Accessing node with no stored value, returning NULL")
    }
    NULL
  }
  new("CacheManager",
    get = get,
    del = nothing,
    chk = false
  )
}

#' Represent a value that has been deleted
#'
#' By default, the value of a node that has already been executed will be set
#' to this function.
noCache <- function(){
  # @param warn Warn if the accessed field does not exist (value was not cached)
  get <- function(warn=TRUE){
    if(warn){
      warning("Attempting to access data that has been deleted, returning NULL")
    }
    NULL
  }
  new("CacheManager",
    get = get,
    del = nothing,
    chk = false
  )
}

#' Store a value in memory
#'
#' @param x Value to be stored
#' @export
#' @examples
#' foo <- 45
#' foo_proxy <- memoryCache(foo)
#' foo
#' foo_proxy@get()
memoryCache <- function(x){
  # FIXME: allow deletion of x, must delete only the LOCAL x 
  # FIXME: allow checking, must check for presence of LOCAL x
  force(x)
  new("CacheManager",
    get = function(...) x,
    del = nothing,
    chk = true
  )
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
#'   foo_@get()
#' }
makeLocalCacher <- function(path){
  if(!dir.exists(path)){
    dir.create(path, recursive=TRUE)
  }
  path <- normalizePath(path)
  # Save x and return a function that can load it
  function(x){
    filename <- file.path(path, paste0('rmonad-', uuid::UUIDgenerate(), ".Rdata"))
    save(x, file=filename) 
    rm(x)
    get <- function(...) {
      load(filename)
      x
    }
    del <- function() {
      file.remove(filename) 
    }
    chk <- function() {
      file.exists(filename)
    }
    new("CacheManager",
      get = get,
      del = del,
      chk = chk
    )
  }
}
