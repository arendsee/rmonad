context("accessor methods")

# This is a pipeline that does almost everything
a <- c(-1,256) %v>% { sqrt(.) %v>% sqrt }
b <- "a" %v>% {"this does stuff"; list(x=1); message("yolo"); paste(., "b")}
ab <- "hi" %__% funnel(a, b) %*>% a

# test_that("Access works for multiple values", {
#   expect_equal(ms_dependents(ab), )
#
#   # ms_code
#   # ms_doc
#   # ms_error
#   # ms_id
#   # ms_mem
#   # ms_meta
#   # ms_nest
#   # ms_nest_depth
#   # ms_notes
#   # ms_OK
#   # ms_parents
#   # ms_prior
#   # ms_summary
#   # ms_time
#   # ms_value
#   # ms_warnings
# })
