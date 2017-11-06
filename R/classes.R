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
    head = "character",
    data = "list"
  )
)
Rmonad <- function(){
  m <- new("Rmonad")
  m <- .new_rmonad_graph(m)
  m@data <- list(RmonadData())
  m
}

setClass(
  "RmonadData",
  representation(
    value      = "CacheManager",
    code       = "character",
    error      = "character",
    warnings   = "character",
    notes      = "character",
    OK         = "logical",
    doc        = "character",
    mem        = "numeric",
    time       = "numeric",
    meta       = "list",
    summary    = "list",
    nest_depth = "integer",
    stored     = "logical"
  )
)
RmonadData <- function(){
  d <- new("RmonadData")
  d@value      <- .default_value()
  d@code       <- .default_code()
  d@error      <- .default_error()
  d@warnings   <- .default_warnings()
  d@notes      <- .default_notes()
  d@OK         <- .default_OK()
  d@doc        <- .default_doc()
  d@mem        <- .default_mem()
  d@time       <- .default_time()
  d@meta       <- .default_meta()
  d@summary    <- .default_summary()
  d@nest_depth <- .default_nest_depth()
  d@stored     <- .default_stored()
  d
}
