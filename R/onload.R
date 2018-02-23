.rmonad_cache <- NULL

.onLoad <- function(libname, pkgname){
  # Set up cache system
  x <- hoardr::hoard()
  x$cache_path_set("rmonad")
  rmonad_cache <<- x

  # Set default options
  # - `rmonad.cache_maxtime`: Maximum runtime in seconds for a node before
  #   deciding to cache the value.  Default: 3.
  options(rmonad.cache_maxtime = 3)
  # - `rmonad.crunch_maxmem`: Maximum memory allowed in a node before the
  #   `crunch` command caches their values. Default: 1e6.
  options(rmonad.crunch_maxmem = 1e6)
  # - 
}
