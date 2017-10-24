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
  m@graph <- igraph::make_empty_graph(directed=TRUE, n=1) %>%
             igraph::set_vertex_attr(name="value", value=list(voidCache()))
  m@head <- 1L
  m_code(m)       <- NULL
  m_error(m)      <- NULL
  m_warnings(m)   <- NULL
  m_notes(m)      <- NULL
  m_OK(m)         <- TRUE
  m_doc(m)        <- NULL
  m_mem(m)        <- NULL
  m_time(m)       <- NULL
  m_meta(m)       <- NULL
  m_nest_depth(m) <- 1
  .m_stored(m)    <- FALSE
  m
}
