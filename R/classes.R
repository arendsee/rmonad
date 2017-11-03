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
    head = "character" #, data = "list"
    # TODO: add rmonad settings (e.g. default cache function)
  )
)

setClass(
  "RmonadData",
  representation(
    code       = "character",
    error      = "character",
    warnings   = "character",
    notes      = "character",
    OK         = "logical",
    doc        = "character",
    mem        = "integer",
    time       = "numeric",
    meta       = "list",
    summary    = "list",
    nest_depth = "integer",
    stored     = "logical"
  )
)
