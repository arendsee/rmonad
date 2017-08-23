#' Make tabular summary of a monadic chain
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @param recurse_nests logical Should the resulting table descend into nested pipelines?
#' @param code logical Should the code by included?
#' @export
mtabulate <- function(m, recurse_nests=TRUE, code=FALSE){
  ms <- as.list(m, recurse_nests)
  d <- do.call(rbind.data.frame, lapply(ms, .mtabulate)) %>%
    as.data.frame
  rownames(d) <- NULL
  if(!code){
    d$code <- NULL
  }
  d
}
.mtabulate <- function(m){
  v <- m_value(m, warn=FALSE)
  list(
    code      = paste(m_code(m), collapse="\n"),
    id        = m_id(m),
    OK        = m_OK(m),
    cached    = has_value(m) && !is_rmonad(v),
    time      = signif(m_time(m)[1], 2),
    space     = m_mem(m),
    is_nested = has_nest(m),
    nbranch   = length(m_branch(m)),
    nnotes    = length(m_notes(m)),
    nwarnings = length(m_warnings(m)),
    error     = length(m_error(m)),
    doc       = length(m_doc(m))
  )
}

#' Tabulates all errors, warnings and notes
#' 
#' @family monad-to-x
#' @param m An Rmonad
#' @param recurse_nests logical Should the resulting table descend into nested pipelines?
#' @export
missues <- function(m, recurse_nests=TRUE){
  ms <- as.list(m)
  .missues <- function(m) {
    type <- c(
              rep.int("error",   length(m_error(m))    ),
              rep.int("warning", length(m_warnings(m)) ),
              rep.int("note",    length(m_notes(m))    )
             )
    issue <- as.character(c(m_error(m), m_warnings(m), m_notes(m)))
    idcol <- rep(m_id(m), length(type))
    list(id=idcol, type=type, issue=issue) 
  }
  do.call(rbind.data.frame, lapply(ms, .missues)) %>%
    as.data.frame  # NOTE: this cast is required, since the above code
                   # silently mishandles the case or a zero-row data
                   # frame (it returns a list).
}

#' Convert a pipeline to Rmarkdown
#'
#' This function is experimental and may change completely in the future
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @param section_prefix A prefix to add to all section headers (mostly for internal use)
#' @export
mreport <- function(m, section_prefix=""){

  template <- paste0(collapse="\n", c(
      "## %s",
      "%s",
      "```{r, eval=FALSE}",
      "%s",
      "```",
      "OK=%s | nparents=%s | nbranches=%s | cached=%s",
      "%s%s%s",
      "%s"
  ))
  lapply(as.list(m, recurse_nests=FALSE),
    function(x) {
      if(section_prefix != ""){
        label <- paste(section_prefix, m_id(x), sep=".")
      } else {
        label <- m_id(x)
      }
      rep <- paste0(sprintf(
        template,
        label,
        .make_message(m_doc(x), x$has_doc(), "  "),
        paste(m_code(x), collapse="\n"),
        m_OK(x), length(m_parents(x)), length(m_branch(x)), has_value(x),
        .make_message(m_error(x), has_error(x), "Error"),
        .make_message(m_warnings(x), has_warnings(x), "Warning"),
        .make_message(m_notes(x), has_notes(x), "Note"),
        .write_result(x)
      ))
      if(has_nest(x)){
        section_prefix <- label
        rep <- paste0(rep, mreport(m_nest(x), section_prefix=section_prefix), collapse="\n")
      }
      rep
    }
  ) %>% unlist %>% paste(collapse="\n")
}
.make_message <- function(x,has,root) {
  if(has){
    paste(root, x, collapse="\n")
  } else {
    ""
  }
}
.write_result <- function(x){
  if(has_value(x)){
    sprintf("```\n%s\n```\n", paste0("R> ", capture.output(print(m_value(x))), collapse="\n"))
  } else {
    ""
  }
}

#' Convert a pipeline to DiagrammeR graph
#'
#' WARNING: this pipeline handles parent/child relationships, but not the
#' subtly different 'branch' relationship. These should be resolvable to
#' parent/child relations.
#'
#' @family monad-to-x
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

  if(any(sapply(ms, m_nest_depth) %>% is.na)){
    m <- recursive_set_nest_depth(m)
  }

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

  # see www.graphviz.org/ for attribute opeions
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
#' @family monad-to-x
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
#' Branches are generated by the `%>^%` operator. The allow a value to be sent
#' into a new branch of the pipeline, which can fail independently, without
#' propagating to other pieces of the program.
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
