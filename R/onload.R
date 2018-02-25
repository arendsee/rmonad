.onLoad <- function(libname, pkgname){

  # Default cache directory
  if(is.null(getOption("rmonad.cache_dir"))){
    options(rmonad.cache_dir = "cache")
  }

  # Default cache function
  if(is.null(getOption("rmonad.cacher"))){
    options(rmonad.cacher = make_cacher())
  }

  # Maximum runtime in seconds for a node before deciding to cache the value.
  # Default: 3.
  if(is.null(getOption("rmonad.cache_maxtime"))){
    options(rmonad.cache_maxtime = 3)
  }

  # - `rmonad.crunch_maxmem`: Maximum memory allowed in a node before the
  #   `crunch` command caches their values. Default: 1e6.
  if(is.null(getOption("rmonad.crunch_maxmem"))){
    options(rmonad.crunch_maxmem = 1e6)
  }
}
