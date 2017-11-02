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
