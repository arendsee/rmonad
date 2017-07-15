#' Make tabular summary of a monadic chain
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @export
mtabulate <- function(m){
  ms <- as.list(m)
  d <- do.call(rbind.data.frame, lapply(ms, .mtabulate)) %>%
    as.data.frame
  d
}
.mtabulate <- function(m){
  list(
    code      = paste(m_code(m), collapse="\n"),
    OK        = m_OK(m),
    cached    = !is.null(m_value(m)),
    time      = signif(m_time(m)[1], 2),
    space     = m_mem(m),
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
#' @export
missues <- function(m){
  ms <- as.list(m)
  cid <- 1L
  .missues <- function(m) {
    type <- c(
              rep.int("error",   length(m_error(m))    ),
              rep.int("warning", length(m_warnings(m)) ),
              rep.int("note",    length(m_notes(m))    )
             )
    issue <- as.character(c(m_error(m), m_warnings(m), m_notes(m)))
    idcol <- rep(cid, length(type))
    cid <<- cid + 1L
    list(id=idcol, type=type, issue=issue) 
  }
  do.call(rbind.data.frame, lapply(ms, .missues)) %>%
    as.data.frame  # NOTE: this cast is required, since the above code
                   # silently mishandles the case or a zero-row data
                   # frame (it returns a list).
}

#' Convert a pipeline to Rmarkdown
#'
#' This is currently a stub. It only pastes the docstrings and code blocks.
#'
#' @family monad-to-x
#' @param m An Rmonad
#' @export
mreport <- function(m){
  lapply(as.list(m),
    function(x) {
      template <- "%s\n```{r, eval=FALSE}\n%s\n```\n"
      paste(sprintf(
        template,
        paste(m_doc(x), collapse="\n"),
        paste(m_code(x), collapse="\n")
      ))
    }
  ) %>% unlist %>% paste(collapse="\n")
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
#' @param ... named functions that act on a monad to produce a scalar. These
#' functions will produce the attributes used in the graph object.
#' @export
#' @examples
#' data(gff)
#' as_dgr_graph(gff$good_result, mem=m_mem, time=m_time)
as_dgr_graph <- function(m, type=NULL, label=NULL, ...){
  ms <- as.list(m)
  funcs <- list(...)
  cols <- lapply(funcs, function(f) sapply(ms, f))
  if(!is.null(type))
    type <- sapply(ms, type)
  if(!is.null(label))
    label <- sapply(ms, label)

  # build the node data frame
  nodes_df <- do.call(
    DiagrammeR::create_node_df,
    list(
      n     = length(ms),
      type  = type,
      label = label
    ) %++% cols
  )
  nodes_df$id <- sapply(ms, m_id)

  # FIXME: this ignores branches
  # build the edges data frame, linking child to parent
  # TODO: add classifications for relationships
  #  %__% creates 'follow' edges, that errors do not propagate past.
  #  %>>% creates 'depend' edges, where errors to propagate
  #  %>_% creates two 'depend' edges
  #  %||% 'reverse-follow'
  #  %|>% 'reverse-depend'
  #  Or perhaps %__% should create separate graphs?
  edges_df <- DiagrammeR::create_edge_df(
    from = lapply(ms, function(x) sapply(m_parents(x), m_id)) %>% unlist,
    to   = lapply(ms, function(x) rep.int(m_id(x), length(m_parents(x)))) %>% unlist,
    rel  = NULL
  )

  # Create graph from node and edge dataframes.
  DiagrammeR::create_graph(
    nodes_df = nodes_df,
    edges_df = edges_df,
    directed = TRUE,
    graph_name = NULL
  ) 
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
  mtab <- mtabulate(m)
  mtab$id <- seq_len(nrow(mtab))

  issues <- merge(mtab, missues(m))[, c("code", "type", "issue")]

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
    Filter(f=.has_branch)  %>%
    lapply(m_branch)       %>%
    lapply(rev)            %>%
    unlist                 %>%
    append(x=list(m))

}
