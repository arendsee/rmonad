#' Conversions to monads
#'
#' These functions convert possibly non-monadic inputs into monads.
#'
#' For each of these functions, failure of any part causes failure of the
#' whole. Any non-monadic inputs will be converted to monads. Any exceptions
#' raised in the inputs will be caught.
#'
#' \code{as_monad} evaluate a single expression into an Rmonad. If the value is
#' already an Rmonad, it will be nested.
#'
#' \code{funnel} evaluates multiple arguments into one Rmonad. It can be used
#' within pipelines to create multi-input nodes (works well with \code{\%*>\%}).
#'
#' \code{combine} takes a list of Rmonads and joins the elements into one
#' Rmonad. The values of the original monadic containers joined into a list in
#' the child Rmonad. The list Rmonads are recorded as the new Rmonad's parents.
#'
#' @param expr An expression
#' @param xs  A list of elements to join into a monad
#' @param doc A docstring to associate with the monad
#' @param key 16 byte raw vector
#' @param desc A description of the monad (usually the producing code)
#' @param keep_history merge the histories of all monads
#' @param env Evaluation environment
#' @param tag Character vector specifying the tag to associate with a node
#' @param lossy logical Should unnesting with record be done?
#' @param ... multiple expressions
#' @name x_to_monad
#' @examples
#' as_monad(stop(1))
#' as_monad(1:10)
#' as_monad(5 %>>% sqrt)
#'
#' ## merge failing inputs 
#' funnel( 1:10, stop(1), sqrt(-3:3) )
#'
#' ## join pipelines
#' b2 <- letters[1:10] %>>% sqrt
#' b3 <- -3:6 %>>% log
#' 1:10 %>% funnel(b2,b3) %>>%
#'   {data.frame(b1=.[[1]], b2=.[[2]], b3=.[[3]])}
#'
#' z <- list(
#'   x = rnorm(10) %>>% sqrt,
#'   y = 1 %>>% colSums
#' )
#' combine(z)
NULL


#' @rdname x_to_monad
#' @export
as_monad <- function(
  expr,
  desc  = NULL,
  tag   = .default_tag(),
  doc   = .default_doc(),
  key   = NULL,
  env   = parent.frame(),
  lossy = FALSE
){
# TODO: 'lossy' is an lousy name, should change to 'nest', or something
# as_monad :: a -> m a

  cacher <- make_cacher()
  if(!is.null(key) && cacher@chk(key)){
    return(cacher@get(key))
  }

  value <- .default_value()
  warns <- .default_warnings()
  fails <- .default_error()
  isOK  <- .default_OK()

  st <- system.time(
    {
      notes <- capture.output(
        {
          value <- withCallingHandlers(
            tryCatch(
              eval(expr, envir=env),
              error = function(e) {
                fails <<- e$message;
                isOK <<- FALSE
              }
            ),
            warning = function(w){
              warns <<- warns %++% w$message
              invokeRestart("muffleWarning")
            }
          )
        },
        type="message"
      )
    },
    gcFirst=FALSE # this kills performance when TRUE
  )

  runtime <- signif(unname(st[1]), 2)

  if(lossy && is_rmonad(value)){
    # If this took a long time to run, then cache the value
    if(runtime > getOption("rmonad.cache_maxtime") && isOK){
      cacher@put(value, key=key)
    }
    return(value)
  }

  ed <- extract_metadata(substitute(expr), env=env)
  expr <- ed$expr
  doc <- ed$docstring
  met <- eval(ed$metadata, envir=env)

  code <- if(is.null(desc)) {
    deparse(substitute(expr))
  } else {
    desc
  }

  if(is.null(key)){
    key <- .digest(code, .get_nest_salt())
  }

  m <- Rmonad(node_id=paste(key, collapse=""))

  if(isOK){
    .single_value(m) <- value
  } else {
    .single_raw_value(m) <- void_cache()
  }

  # `tag` splits the tags on '/'
  m <- tag(m, tag)

  # These accessors do the right thing (don't mess with them)
  .single_code(m)       <- code
  .single_key(m)        <- key
  .single_error(m)      <- fails
  .single_warnings(m)   <- warns
  .single_notes(m)      <- notes
  .single_OK(m)         <- isOK
  .single_doc(m)        <- doc
  .single_mem(m)        <- as.integer(object.size(value))
  .single_time(m)       <- runtime
  .single_meta(m)       <- met
  .single_summary(m)    <- .default_summary()
  .single_depth(m)      <- .default_depth()
  .single_nest_depth(m) <- .default_nest_depth()
  .single_stored(m)     <- .default_stored()

  m <- apply_rewriters(m, met)

  # If this took a long time to run, then cache the value
  if(runtime >= getOption("rmonad.cache_maxtime") && isOK){
    cacher@put(m, key=key)
  }

  m

}


#' @rdname x_to_monad
#' @export
funnel <- function(..., env=parent.frame(), keep_history=TRUE){
# funnel :: [Rexpr] -> m [*]

  # NOTE: don't deparse '...' to get labels, this leads to massive performance
  # penalties when data is piped in (e.g. deparsing a dataframe).

  ms <- .funnel_ms(
    es = substitute(alist(...))[-1],
    env=env
  )

  desc <- deparse(match.call())

  combine(ms, keep_history=keep_history, desc=desc)

}

# internal function, for building from a list of expressions
.funnel_sub <- function(es, env=parent.frame(), ...){

  ms <- .funnel_ms(es, env)

  combine(ms, ...)
}
.funnel_ms <- function(es, env=parent.frame()){
  lapply(
    es,
    # how to stringify x
    function(x) {

      # if x is a call, deparse it
      desc <- if(is.call(x)){
        deparse(x)
      }
      # the substitution in funnel will evaluated these
      else if(is.atomic(x) && length(x) == 1) {
        as.character(x)
      }
      else if(is.name(x)){
        as.character(x)
      }
      # anything else, will be data passed in from the pipe
      else {
        "."
      }

      as_monad(eval(x, env), desc=desc, lossy=TRUE)
    }
  )
}


#' @rdname x_to_monad
#' @export
combine <- function(xs, keep_history=TRUE, desc=.default_code()){
# combine :: [m *] -> m [*]

  if(!all(vapply(FUN.VALUE=logical(1), xs, is_rmonad))){
    stop("'combine' works only on lists of Rmonad objects")
  }

  # store all values (even if failing, in which case should be NULL)
  value <- lapply(xs, function(x){
    if(get_OK(x, x@head)){
      .single_value(x, warn=FALSE)
    } else {
      NULL
    }
  })

  # When combining multiple nodes, the new key is the XOR of the code digest
  # against the keys of all the parents.
  # `desc` will hold the full term: e.g. `funnel(x = 2, y = whatever)`
  # This ensures the key will change if the order of arguments changes (which
  # is important when one or more of them are positional.
  parent_keys <- lapply(xs, function(x) get_key(x, x@head)[[1]])
  key <- .digest(parent_keys, desc)

  # make a new monad that is the child of all monads in the input list
  out <- as_monad(value, key=key)

  # remove cached value of parents if they were passing AND if they have NO tag
  xs <- lapply(xs, function(x){
    if(get_OK(x, x@head) && !has_tag(x, x@head)){
      .single_delete_value(x)
    } else {
      x
    }
  })

  .single_parents(out) <- xs
  .single_time(out) <- .default_time()

  # monad is passing if all parents are cool
  .single_OK(out) <- all(vapply(FUN.VALUE=logical(1), xs, .single_OK))

  if(!is.null(desc)){
    .single_code(out) <- desc 
  }

  out 
}
