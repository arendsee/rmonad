#' Represent a value that has not been set
#'
#' This is the default value of RmonadData@value. It should always be replaced
#' shortly after the object is created, thus should only be encountered if 1)
#' the user is directly creating RmonadData objects (in which case they should
#' be spoken to sternly) or 2) there is a bug in rmonad.
#'
#' @return A function that represents a void, uncached value
#' @export
#' @family cache
void_cache <- function(){
  # @param warn Warn if the accessed field does not exist (value was not cached)
  get <- function(warn=TRUE){
    if(warn){
      warning("Accessing node with no stored value, returning 'NULL'")
    }
    NULL
  }
  new("ValueManager",
    in_memory = TRUE,
    get = get,
    del = nothing,
    chk = false
  )
}

#' Represent a dummy value for a node downstream of a failing node 
#'
#' Returns a ValueManager that represents a dummy value for a node downstream
#' of a failing node. Unlike \code{void_cache}, this presence of this manager
#' in a pipeline is not pathological, so does not raise a warning by default.
#'
#' @return A function that represents an unrun node
#' @export
#' @family cache
fail_cache <- function(){
  get <- function(warn=FALSE){
    if(warn){
      warning("Accessing node downstream of failing function, returning 'NULL'")
    }
    NULL
  }
  new("ValueManager",
    in_memory = TRUE,
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
#' @family cache
no_cache <- function(){
  # @param warn Warn if the accessed field does not exist (value was not cached)
  get <- function(warn=TRUE){
    if(warn){
      warning("Attempting to access data that has been deleted, returning 'NULL'")
    }
    NULL
  }
  new("ValueManager",
    in_memory = TRUE,
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
#' @family cache
#' @examples
#' foo <- 45
#' foo_proxy <- memory_cache(foo)
#' foo
#' foo_proxy@get()
memory_cache <- function(x){
  # FIXME: allow deletion of x, must delete only the LOCAL x 
  # FIXME: allow checking, must check for presence of LOCAL x
  force(x)
  new("ValueManager",
    in_memory = TRUE,
    get = function(...) x,
    del = nothing,
    chk = true
  )
}

#' Make Cacher object
#'
#' @param f_path A function for finding the directory in which to cache results
#' @param f_save function of x and filename that saves x to the path filename
#' @param f_get function of filename that retrieves the cached data
#' @param f_del function of filename that deletes the cached data
#' @param f_ext function of class(x) that determines the filename extension
#' @return A function that builds a local cache function for a value
#' @export
#' @family cache
make_cacher <- function(
  f_path = function() getOption("rmonad.cache_dir"),
  f_save = saveRDS,
  f_get  = readRDS,
  f_del  = unlink,
  f_ext = function(cls) ".Rdata" 
){
  if(!dir.exists(f_path())){
    dir.create(f_path(), recursive=TRUE)
  }

  get_files <- function(key){
    list.files(f_path(), sprintf("^%s\\..*", key), full.names=TRUE)
  }

  chk = function(key) {
    # Or exactly one cached file has this key (with any extension)
    length(get_files(key)) == 1
  }

  get = function(key, warn=FALSE, ...) {
    if(chk(key)){
      f_get(get_files(key), ...)
    } else {
      stop("Cannot uncache this value") 
    }
  }

  put = function(x, key) {
    extension <- f_ext(class(x))
    filename <- file.path(f_path(), paste0(key, extension))
    f_save(x, filename)
  }

  del = function(key, ...) f_del(get_files(key), ...)

  new("Cacher",
    chk = chk,
    put = put,
    get = get,
    del = del,
    bld = function(key){
      new("ValueManager",
        in_memory = FALSE,
        get = function() get(key),
        del = function() del(key),
        chk = function() chk(key)
      )
    }
  )
}

#' Clear cached values and delete temporary files
#'
#' @param m Rmonad object
#' @param index indices to clear (all indices by default)
#' @return Rmonad object
#' @export
#' @family cache
#' @examples
#' 256 %v>% sqrt %>>% sqrt %>>% sqrt  -> m
#' m
#' clear_cache(m)
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
#' @family cache
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
  # @param m An Rmonad object
  # @param tag A tag for quick access to the cached node
  function(m, tag=.default_tag()){
    # lossy, so as_monad will not create extra nesting
    m <- as_monad(m, lossy=TRUE, desc=deparse(substitute(m)))
    .single_raw_value(m) <- cacher(.single_value(m))
    .single_stored(m) <- preserve
    .single_tag(m) <- tag
    m
  }
}

.digest <- function(...){
    lapply(list(...), serialize, connection=NULL) %>% digest::digest(algo='md5')
}

crunch <- function(m){
  # STUB: find all nodes that require a lot of memory and cache their values
}

cache <- function(cacher, key, eval, func, args){  

  x <- if(cacher$chk(key)){
    x <- get(key)
  } else {
    
  }

}
