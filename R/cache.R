#' Represent a value that has not been set
#'
#' This is the default value of RmonadData@value. It should always be replaced
#' shortly after the object is created, thus should only be encountered if 1)
#' the user is directly creating RmonadData objects (in which case they should
#' be spoken to sternly) or 2) there is a bug in rmonad.
#'
#' @return A function that represents a void, uncached value
#' @export
void_cache <- function(){
  # @param warn Warn if the accessed field does not exist (value was not cached)
  get <- function(warn=TRUE){
    if(warn){
      warning("Accessing node with no stored value, returning 'NULL'")
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
#'
#' @return A function that represents a deleted value 
#' @export
no_cache <- function(){
  # @param warn Warn if the accessed field does not exist (value was not cached)
  get <- function(warn=TRUE){
    if(warn){
      warning("Attempting to access data that has been deleted, returning 'NULL'")
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
#' @return A function that returns a value stored in memory 
#' @export
#' @examples
#' foo <- 45
#' foo_proxy <- memory_cache(foo)
#' foo
#' foo_proxy@get()
memory_cache <- function(x){
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
#' @param path A directory in which to cache results. A temporary directory is
#' created if none is given.
#' @return A function that builds a local cache function for a value
#' @export
#' @examples
#' \dontrun{
#'   foo <- 45
#'   cacher <- make_local_cacher()
#'   foo_ <- cacher(45)
#'   rm(foo)
#'   foo_@get()
#' }
make_local_cacher <- function(path=tempdir()){
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
      unlink(filename) 
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

#' Clear cached values and delete temporary files
#'
#' @param m Rmonad object
#' @param index indices to clear (all indices by default)
#' @return Rmonad object
#' @export
clear_cache <- function(m, index=.get_ids(m)){
  for(cc in .get_many_attributes(m, attribute='value', index=index)){
    cc@del()
  }
  .set_many_attributes(
    m,
    attribute = 'value',
    value     = lapply(seq_along(index), function(x) no_cache()),
    index     = index
  )
}

#' Make a function that takes an Rmonad and recaches it
#'
#' @param cacher A function of a data value
#' @param preserve logical Should the cached value be preserved across bind operations?
#' @return A function that swaps the cache function of an Rmonad
#' @export
#' @examples
#' \dontrun{
#'   recacher <- make_recacher(make_local_cacher())
#'   m <- iris %>>% summary %>% recacher
#'   # load the data from a local file
#'   .single_value(m)
#'
#'   recacher <- make_recacher(memory_cache)
#'   m <- iris %>>% summary %>% recacher
#'   # load the data from memory
#'   .single_value(m)
#' }
#'
#' add1 <- function(x) x+1
#' add2 <- function(x) x+2
#' add3 <- function(x) x+3
#' cc <- make_recacher(make_local_cacher())
#' 3 %>>% add1 %>% cc %>>% add2 %>>% add3 -> m
#' m
make_recacher <- function(cacher, preserve=TRUE){
  # TODO: should check that meta$cache is a proper caching function
  # @param m An Rmonad object
  function(m){
    .single_raw_value(m) <- cacher(.single_value(m))
    .single_stored(m) <- preserve
    m
  }
}
