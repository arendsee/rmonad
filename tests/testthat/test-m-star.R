context("accessor methods")

# This is a pipeline that does almost everything
a <- c(-1,256) %v>% { sqrt(.) %v>% { list(summarize=identity); sqrt(.) } }
b <- "a" %v>% {"this does stuff"; list(x=1); message("yolo"); paste(., "b")}
ab <- "hi" %__% funnel(a, b) %*>% (function(x,y) { stop("die die") })

test_that("Access works for multiple values", {
  expect_equal(
    ms_dependents(ab),
    list(
      integer(0), #1
      3,          #2
      8,          #3
      7,          #4
      6,          #5 -- this was incorrectly [5,6]
      integer(0), #6
      8,          #7
      9,          #8
      integer(0)  #9
    )
  )

  expect_equal(
    ms_code(ab)[c(1,6)], list('"hi"', "sqrt")
  )

  expect_equal(
    ms_doc(ab)[c(3,9)], list("this does stuff", NULL)
  )

  expect_equal(
    ms_error(ab)[c(1,9)], list(NULL, "die die")
  )

  expect_equal(
    ms_id(ab), 1:9
  )

  expect_true(
    is.numeric(ms_mem(ab)) && ms_mem(ab) > 0 && length(ms_mem(ab)) == 9
  )

  expect_equal(
    ms_meta(ab)[c(1,3)], list(list(), list(x=1))
  )

  expect_equal(
    ms_nest(ab)[c(1,7)], list(integer(0), 6)
  )

  expect_equal(
    ms_nest_depth(ab), c(1,1,1,1,2,2,1,1,1)
  )

  expect_equal(
    ms_notes(ab),
    list(
      character(0),
      character(0),
      "yolo",
      character(0),
      character(0),
      character(0),
      character(0),
      character(0), # Was NULL under previous buggy state
      character(0)
    )
  )

  expect_equal(
    ms_OK(ab), c(T,T,T,T,T,T,T,T,F)
  )

  expect_equal(
    ms_parents(ab),
    list(
      integer(0), #1
      integer(0), #2
      2,          #3
      integer(0), #4
      7,          #5
      5,          #6
      4,          #7
      c(3,7),     #8
      8           #9
    )
  )

  expect_equal(
    ms_prior(ab),
    list(
      integer(0), #1
      integer(0), #2
      integer(0), #3
      integer(0), #4
      integer(0), #5
      integer(0), #6
      integer(0), #7
      1,          #8
      integer(0)  #9
    )
  )

  expect_equal(
    ms_summary(ab),
    list(
      NULL,
      NULL,
      NULL,
      4,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
    )
  )

  expect_true(
    is.numeric(ms_time(ab)) &&
    is.na(ms_time(ab)[8])    # This is the container created by funnel
  )

  expect_equal(
    ms_value(ab, warn=FALSE),
    list(
      "hi",       #1
      "a",        #2
      NULL,       #3
      c(-1, 256), #4
      c(NaN, 16), #5
      NULL,       #6
      NULL,       #7
      NULL,       #8
      list(       #9
        c(NaN, 4),
        "a b"
      )
    )
  )

  expect_equal(
    ms_warnings(ab),
    list(
      character(0),
      character(0),
      character(0),
      character(0),
      "NaNs produced",
      character(0),
      character(0),
      character(0),
      character(0)
    )
  )

})
