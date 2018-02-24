.onLoad <- function(libname, pkgname){

  setOptionIfUnset <- function(opt, val){
    if(is.null(getOption(opt))){
      options(opt = val)
    }
  }

  # Default cache deirectory
  setOptionIfUnset("rmonad.cache_dir", "cache")

  # Default cache function
  setOptionIfUnset("rmonad.cacher", make_cacher())

  # Maximum runtime in seconds for a node before deciding to cache the value.
  # Default: 3.
  setOptionIfUnset("rmonad.cache_maxtime", 3)

  # - `rmonad.crunch_maxmem`: Maximum memory allowed in a node before the
  #   `crunch` command caches their values. Default: 1e6.
  setOptionIfUnset("rmonad.crunch_maxmem", 1e6)
}
