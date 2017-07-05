#' Record a monad's current value
#'
#' @param m An Rmonad
#' @export
record <- function(m){
  m@stage@x <- m@x 
  m
}

#' Return the value of a possibly monadic input
#'
#' If the monad is failing, raise an error. Otherwise, write the final value.
#'
#' @param x rmonad or whatever
#' @export
esc <- function(x){
  m <- as_rmonad(x)
  if(m@OK){
    if(length(m@x) == 1){
      m@x[[1]]
    } else {
      m@x
    }
  } else {
    msg <- unlist(m@stage@errors) %>% paste(collapse="\n")
    msg <- paste0('The call "', m@stage@code, '" failed: \n  ', msg)
    stop(msg, call.=FALSE)
  }
}
