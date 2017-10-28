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
    head = "integer"
    # TODO: add rmonad settings (e.g. default cache function)
  )
)

Rmonad <- function(){
  m <- new("Rmonad")
  m@graph <- .new_rmonad_graph()
  m@head <- .default_head()
  m <- .set_raw_value(m, list(voidCache()))
  m_code(m)       <- .default_code()
  m_error(m)      <- .default_error()
  m_warnings(m)   <- .default_warnings()
  m_notes(m)      <- .default_notes()
  m_OK(m)         <- .default_OK()
  m_doc(m)        <- .default_doc()
  m_mem(m)        <- .default_mem()
  m_time(m)       <- .default_time()
  m_meta(m)       <- .default_meta()
  m_nest_depth(m) <- .default_nest_depth()
  m_summary(m)    <- .default_summary()
  .m_stored(m)    <- .default_stored()
  m
}
