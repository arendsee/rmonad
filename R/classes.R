setClass(
  "CacheManager",
  representation(
    get = "function",
    del = "function",
    chk = "function"
  )
)

setOldClass("igraph")
setClass(
  "Rmonad",
  representation(
    graph = "igraph",
    head = "character"
    # TODO: add rmonad settings (e.g. default cache function)
  )
)

Rmonad <- function(){
  m <- new("Rmonad")
  m <- .new_rmonad_graph(m)
  m <- .set_raw_value(m, list(voidCache()))
  .single_code(m)       <- .default_code()
  .single_error(m)      <- .default_error()
  .single_warnings(m)   <- .default_warnings()
  .single_notes(m)      <- .default_notes()
  .single_OK(m)         <- .default_OK()
  .single_doc(m)        <- .default_doc()
  .single_mem(m)        <- .default_mem()
  .single_time(m)       <- .default_time()
  .single_meta(m)       <- .default_meta()
  .single_nest_depth(m) <- .default_nest_depth()
  .single_summary(m)    <- .default_summary()
  .single_stored(m)     <- .default_stored()
  m
}
