#' get, set, and append rmonad fields
#'
#' @param m the rmonad
#' @param value value to replace or append current value
#' @param warn Warn if the accessed field does not exist (value was not cached)
#' @name rmonad_accessors
NULL



#' @rdname rmonad_accessors
#' @export
is_rmonad <- function(m) {
  all(class(m) == c("Rmonad", "R6"))
}

# internal utility for generating error messages when accessing a non-Rmonad
.m_check <- function(m) {
  if(!is_rmonad(m)){
    msg="Expected an Rmonad object, got %s"
    stop(sprintf(msg, class(m)))
  }
}

m_delete_value <- function(m) {
  m$delete_value()
  m
}

has_code     <- function(m) m$has_code()
has_error    <- function(m) m$has_error()
has_doc      <- function(m) m$has_doc()
has_warnings <- function(m) m$has_warnings()
has_notes    <- function(m) m$has_notes()
has_parents  <- function(m) m$has_parents()
has_nest     <- function(m) m$has_nest()
has_branch   <- function(m) m$has_branch()
has_meta     <- function(m) m$has_meta()
has_time     <- function(m) m$has_time()
has_mem      <- function(m) m$has_mem()
has_value    <- function(m) m$has_value()


#' @rdname rmonad_accessors
#' @export
m_parents <- function(m) {
  .m_check(m)
  m$get_parents()
}

#' @rdname rmonad_accessors
#' @export
m_nest <- function(m) {
  .m_check(m)
  m$get_nest()
}

#' @rdname rmonad_accessors
#' @export
m_nest_depth <- function(m) {
  .m_check(m)
  m$get_nest_depth()
}

#' @rdname rmonad_accessors
#' @export
m_value <- function(m, warn=TRUE){
  .m_check(m)
  m$get_x()
}

#' @rdname rmonad_accessors
#' @export
m_id <- function(m) {
  .m_check(m)
  m$set_id()
}

#' @rdname rmonad_accessors
#' @export
m_OK <- function(m) {
  .m_check(m)
  m$get_OK()
}

#' @rdname rmonad_accessors
#' @export
m_code <- function(m) {
  .m_check(m)
  m$get_code()
}

#' @rdname rmonad_accessors
#' @export
m_error <- function(m) {
  .m_check(m)
  m$get_error()
}

#' @rdname rmonad_accessors
#' @export
m_warnings <- function(m) {
  .m_check(m)
  m$get_warnings()
}

#' @rdname rmonad_accessors
#' @export
m_notes <- function(m) {
  .m_check(m)
  m$get_notes()
}

#' @rdname rmonad_accessors
#' @export
m_doc <- function(m) {
  .m_check(m)
  m$get_doc()
}

#' @rdname rmonad_accessors
#' @export
m_meta <- function(m) {
  .m_check(m)
  m$get_meta()
}

#' @rdname rmonad_accessors
#' @export
m_time <- function(m) {
  .m_check(m)
  m$get_time()
}


#' @rdname rmonad_accessors
#' @export
m_mem <- function(m) {
  .m_check(m)
  m$get_mem()
}

#' @rdname rmonad_accessors
#' @export
m_branch   <- function(m) {
  .m_check(m)
  m$get_branch()
}


#' @rdname rmonad_accessors
#' @export
`m_OK<-` <- function(m, value) {
  .m_check(m)
  m$set_OK(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_value<-` <- function(m, value) {
  .m_check(m)
  m$set_x(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_parents<-` <- function(m, value) {
  .m_check(m)
  m$set_parents(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_nest<-` <- function(m, value) {
  .m_check(m)
  m$set_nest(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_nest_depth<-` <- function(m, value) {
  .m_check(m)
  m$set_nest_depth(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_code<-` <- function(m, value) {
  .m_check(m)
  m$set_code(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_error<-` <- function(m, value) {
  .m_check(m)
  m$set_error(value)
  m
}


#' @rdname rmonad_accessors
#' @export
`m_warnings<-` <- function(m, value) {
  .m_check(m)
  m$set_warnings(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_notes<-` <- function(m, value) {
  .m_check(m)
  m$set_notes(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_doc<-` <- function(m, value) {
  .m_check(m)
  m$set_doc(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_meta<-` <- function(m, value) {
  .m_check(m)
  m$set_meta(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_time<-` <- function(m, value) {
  .m_check(m)
  m$set_time(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_mem<-` <- function(m, value) {
  .m_check(m)
  m$set_mem(value)
  m
}

#' @rdname rmonad_accessors
#' @export
`m_branch<-` <- function(m, value) {
  .m_check(m)
  m$set_branch(value)
  m
}



#' @rdname rmonad_accessors
#' @export
app_warnings <- function(m, value) {
  .m_check(m)
  m$app_warnings(value)
  m
}

#' @rdname rmonad_accessors
#' @export
app_notes <- function(m, value) {
  .m_check(m)
  m$app_notes(value)
  m
}

#' @rdname rmonad_accessors
#' @export
app_branch <- function(m, value) {
  .m_check(m)
  m$app_branch(value)
  m
}

#' @rdname rmonad_accessors
#' @export
app_parents <- function(m, value) {
  .m_check(m)
  m$app_parents(value)
  m
}
