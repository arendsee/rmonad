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
  m
}
