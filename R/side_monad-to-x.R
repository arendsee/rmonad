#' Make tabular summary of a pipeline
#'
#' @family from_Rmonad
#' @param m An Rmonad
#' @param code logical Should the code by included?
#' @export
mtabulate <- function(m, code=FALSE){
  data.frame(
    code      = igraph::V(m@graph)$code %>% paste(collapse="\n"),
    id        = igraph::V(m@graph) %>% as.numeric,
    OK        = igraph::V(m@graph)$OK,
    cached    = igraph::V(m@graph)$value %>% sapply(function(x) x@chk()),
    time      = igraph::V(m@graph)$time %>% { signif(.[1], 2) },
    space     = igraph::V(m@graph)$mem,
    # is_nested = has_nest(m),
    # nbranch   = length(m_branch(m)),
    nnotes    = igraph::V(m@graph)$notes    %>% sapply(length),
    nwarnings = igraph::V(m@graph)$warnings %>% sapply(length),
    error     = igraph::V(m@graph)$error    %>% sapply(length),
    doc       = igraph::V(m@graph)$doc      %>% sapply(length)
  ) %>% {
    if(!code)
      .$code <- NULL
    .
  }
}

#' Tabulates all errors, warnings and notes
#' 
#' @family from_Rmonad
#' @param m An Rmonad
#' @param recurse_nests logical Should the resulting table descend into nested pipelines?
#' @export
missues <- function(m, recurse_nests=TRUE){
  ids      <- igraph::V(m@graph) %>% as.numeric
  error    <- igraph::V(m@graph)$error    %>% unlist
  warnings <- igraph::V(m@graph)$warnings %>% unlist
  notes    <- igraph::V(m@graph)$notes    %>% unlist
  data.frame(
    id = ids, 
    type = c(
      rep("error", length(error)),
      rep("warning", length(warnings)),
      rep("note", length(notes))
    ),
    issue = c(error, warnings, notes)
  )
}

#' Convert a pipeline to Rmarkdown
#'
#' STUB
#'
#' @family from_Rmonad
#' @param m An Rmonad
#' @export
mreport <- function(m){
  stop("NOT IMPLEMENTED")
}

#' Convert a pipeline to DiagrammeR graph
#'
#' Convert an Rmonad to a DiagrammeR graph. Branches, which are made with the
#' `%>^%` operator, will not be included in the resulting graph. This is a bug
#' which I will resolve in the near future.
#'
#' @family from_Rmonad
#' @param m An Rmonad
#' @param type a function that will produce the type column for DiagrammeR
#' @param label a function that will produce the label column for DiagrammeR
#' @param color a function that sets the color of each node
#' @param ... named functions that act on a monad to produce a scalar. These
#' functions will produce the attributes used in the graph object.
#' @export
#' @examples
#' data(gff)
#' g <- as_dgr_graph(gff$good_result, mem=m_mem, time=m_time)
as_dgr_graph <- function(m, type=NULL, label=NULL, color=NULL, ...){
  ms <- as.list(m)

  funcs <- list(...)
  cols <- lapply(funcs, function(f) sapply(ms, f))
  if(!is.null(type))
    type <- sapply(ms, type)
  if(!is.null(label))
    label <- sapply(ms, label)
 
  .check_length(type)
  .check_length(label)
  lapply(cols, .check_length)
 
  fillcolor <-
  if(is.function(color)){
    sapply(ms, color)
  } else if(is.null(color)){
    NULL
  } else {
    stop("The 'color' parameter in as_dgr_graph must be either NULL or a function")
  }

  # build the node data frame
  nodes_df <- do.call(
    DiagrammeR::create_node_df,
    list(
      n     = length(ms),
      type  = type,
      label = label,
      rank  = sapply(ms, m_nest_depth)
    ) %++% cols
  )
  nodes_df$id <- sapply(ms, m_id)
  nodes_df$fillcolor <- fillcolor

  edges_df_pc <- DiagrammeR::create_edge_df(
    from    = lapply(ms, function(x) sapply(m_parents(x), m_id)) %>% unlist,
    to      = lapply(ms, function(x) rep.int(m_id(x), length(m_parents(x)))) %>% unlist,
    f_depth = lapply(ms, function(x) sapply(m_parents(x), m_nest_depth)) %>% unlist,
    t_depth = lapply(ms, function(x) rep.int(m_nest_depth(x), length(m_parents(x)))) %>% unlist,
    rel     = "depend"
  )
  edges_df_nest <- DiagrammeR::create_edge_df(
    from  = sapply(ms, function(x) if(has_nest(x)) m_id(m_nest(x)) else NA ),
    to    = sapply(ms, m_id),
    f_depth = lapply(ms, function(x) sapply(m_parents(x), m_nest_depth)) %>% unlist,
    t_depth = lapply(ms, function(x) rep.int(m_nest_depth(x), length(m_parents(x)))) %>% unlist,
    rel   = "nest"
  )
  edges_df_nest <- edges_df_nest[sapply(ms, has_nest), ]

  edges_df <- rbind(edges_df_pc, edges_df_nest)

  has_prior <- sapply(ms, function(x) if (x$has_prior()) m_id(x) else NA) %>% {.[!is.na(.)]}
  is_prior <- sapply(ms, function(x) if (x$has_prior()) m_id(x$get_prior()) else NA) %>% {.[!is.na(.)]}

  # Check for `%__%` operators
  if(length(has_prior > 0)){
    edges_df_prior <- DiagrammeR::create_edge_df(
      from  = has_prior,
      to    = is_prior,
      f_depth = rep(0, length(has_prior)),
      t_depth = rep(0, length(has_prior)),
      rel   = "prior"
    )
    edges_df <- rbind(edges_df,    edges_df_prior)
  }

  edges_df$rel <- ifelse(
    (edges_df$t_depth != edges_df$f_depth) & edges_df$rel == 'depend',
    'transitive',
    edges_df$rel
  )

  # Create graph from node and edge dataframes.
  DiagrammeR::create_graph(
    nodes_df   = nodes_df,
    edges_df   = edges_df,
    directed   = TRUE,
    graph_name = NULL
  )
}
.check_length <- function(x){
  if(!is.null(x) && any(sapply(x, length) != 1)){
    stop("All attributes fields must have length 1")
  }
}

#' Returns the value of a monad holds
#'
#' If the monad is in the passing state, return the wrapped value. Otherwise,
#' raise an appropriate error.
#'
#' Regardless of pass/fail status, \code{esc} raises all collected warnings and
#' prints all messages. Terminating a monadic sequence with \code{esc} should
#' obtain a result very close to running the same code outside the monad. The
#' main difference is that Rmonad appends the toplevel code that generated the
#' error.
#'
#' @family from_Rmonad
#' @param m An Rmonad
#' @param quiet If TRUE, print the exact messages that are raised, without
#'        extra context. 
#' @export 
esc <- function(m, quiet=FALSE){
  mtab <- mtabulate(m, recurse_nests=TRUE, code=TRUE)

  issues <- missues(m, recurse_nests=TRUE) %>%
    { merge(mtab, .)[, c("code", "type", "issue")] }

  if(quiet){
    fw <- .quiet_warning
    fn <- .quiet_note
    fe <- .quiet_error
  } else {
    fw <- .unquiet_warning
    fn <- .unquiet_note
    fe <- .unquiet_error
  }

  for(i in seq_len(nrow(issues))){
    # raise warnings, with contextual information
    if(issues[i, "type"] == "warning"){
      fw(issues[i, "code"], issues[i, "issue"])
    }
    # pass messages verbatim
    if(issues[i, "type"] == "note"){
      fn(issues[i, "code"], issues[i, "issue"])
    }
  }
  if(! m_OK(m)){
    fe(m_code(m), m_error(m))
  }

  m_value(m)
}

.quiet_warning <- function(code, msg) warning(msg, call.=FALSE)
.quiet_note    <- function(code, msg) message(msg)
.quiet_error   <- function(code, msg) stop(msg, call.=FALSE)

.unquiet_warning <- function(code, msg) {
  warning("in '", code, "': ", msg, call.=FALSE)
}
.unquiet_note <- function(code, msg) {
  message(msg)
}
.unquiet_error <- function(code, msg) {
  stop(paste0('in "', code, '":\n  ', msg), call.=FALSE)
}


#' Return each independent branch of the pipeline
#'
#' Branches are generated by the \code{\%>^\%} operator. The allow a value to
#' be sent into a new branch of the pipeline, which can fail independently,
#' without propagating to other pieces of the program.
#'
#' @param m An Rmonad
#' @return A list of Rmonads, one for each branch
#' @export
unbranch <- function(m){

  as.list(m)               %>%
    Filter(f=has_branch)   %>%
    lapply(m_branch)       %>%
    lapply(rev)            %>%
    unlist                 %>%
    append(x=list(m))

}
