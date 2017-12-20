#' Apply rewriters to an Rmonad
#'
#' Rewriters are functions stored in an Rmonad's metadata list that operate on
#' an Rmonad after it has evaluated its code.
#'
#' @param x The Rmonad
#' @param meta A metadata list
apply_rewriters <- function(x, meta=.single_meta(x)){

  if(is.function(meta$format_warnings) && has_warnings(x, index=x@head)){
    .single_warnings(x) <- meta$format_warnings(.single_value(x), .single_warnings(x))
  }

  if(is.function(meta$format_error) && has_error(x, index=x@head)){
    .single_error(x) <- meta$format_error(.single_value(x), .single_error(x))
  }

  if(is.function(meta$format_notes) && has_notes(x, index=x@head)){
    .single_notes(x) <- meta$format_notes(.single_value(x), .single_notes(x))
  }

  if(is.function(meta$summarize) && .single_OK(x)){
    .single_summary(x) <- list(meta$summarize(.single_value(x)))
  }
  if(is.list(meta$summarize) &&
     all(vapply(FUN.VALUE=logical(1), meta$summarize, is.function)) &&
     .single_OK(x)
  ){
    .single_summary(x) <- lapply(meta$summarize, function(f) f(.single_value(x)))
  }

  if(is.function(meta$cache) && .single_OK(x)){
    x <- make_recacher(meta$cache)(x)
  }

  if(is.function(meta$log)){
    meta$log(x, passing=.single_OK(x))
  }

  x
}
