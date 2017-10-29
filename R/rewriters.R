# TODO: document this

#' Apply rewriters to an Rmonad
#'
#' Rewriters are functions stored in an Rmonad's metadata list that operate on
#' an Rmonad after it has evaluated its code.
#'
#' @param x The Rmonad
#' @param meta A metadata list
apply_rewriters <- function(x, meta=m_meta(x)){

  if(is.function(meta$format_warnings) && has_warnings(x, index=x@head)){
    m_warnings(x) <- meta$format_warnings(m_value(x), m_warnings(x))
  }

  if(is.function(meta$format_error) && has_error(x, index=x@head)){
    m_error(x) <- meta$format_error(m_value(x), m_error(x))
  }

  if(is.function(meta$format_notes) && has_notes(x, index=x@head)){
    m_notes(x) <- meta$format_notes(m_value(x), m_notes(x))
  }

  if(is.function(meta$summarize) && m_OK(x)){
    m_summary(x) <- meta$summarize(m_value(x))
  }

  if(is.function(meta$cache) && m_OK(x)){
    meta$cache(m_value(x))
  }

  if(is.function(meta$log)){
    meta$log(x, passing=m_OK(x))
  }

  x
}
