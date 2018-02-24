.onLoad <- function(libname, pkgname){
  # Default cache deirectory
  options(rmonad.cache_dir = "cache")

  # Maximum runtime in seconds for a node before deciding to cache the value.
  # Default: 3.
  options(rmonad.cache_maxtime = 3)

  # - `rmonad.crunch_maxmem`: Maximum memory allowed in a node before the
  #   `crunch` command caches their values. Default: 1e6.
  options(rmonad.crunch_maxmem = 1e6)
}
