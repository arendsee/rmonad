context("vectorized accessor methods")

# This is a pipeline that does almost everything
a <- c(-1,256) %v>% { sqrt(.) %v>% { "summarized"; list(summarize=identity); sqrt(.) } }
b <- "a" %v>% {"this does stuff"; list(x=1); message("yolo"); paste(., "b")}
ab <- "hi" %__% funnel(a, b) %*>% (function(x,y) { stop("die die") })

test_that("Access works for multiple values", {
  expect_equal(
    get_dependents(ab),
    list(
      .default_id(), #1
      3,             #2
      8,             #3
      7,             #4
      6,             #5 -- this was incorrectly [5,6]
      .default_id(), #6
      8,             #7
      9,             #8
      .default_id()  #9
    )
  )

  expect_equal(
    get_code(ab)[c(1,2)], list('"hi"', '"a"')
  )

  expect_equal(
    get_doc(ab)[c(3,9)], list("this does stuff", .default_doc())
  )

  expect_equal(
    get_error(ab)[c(1,9)], list(.default_error(), "die die")
  )

  expect_equal(
    get_id(ab), 1:9
  )

  expect_true(
    is.numeric(get_mem(ab)) && get_mem(ab) > 0 && length(get_mem(ab)) == 9
  )

  expect_equal(
    get_meta(ab)[c(1,3)], list(list(), list(x=1))
  )

  expect_equal(
    get_nest(ab)[c(1,7)], list(.default_id(), 6)
  )

  expect_equal(
    get_nest_depth(ab), c(1,1,1,1,2,2,1,1,1)
  )

  expect_equal(
    get_notes(ab),
    list(
      .default_notes(),
      .default_notes(),
      "yolo",
      .default_notes(),
      .default_notes(),
      .default_notes(),
      .default_notes(),
      .default_notes(), # Was NULL under previous buggy state
      .default_notes()
    )
  )

  expect_equal(
    get_OK(ab), c(T,T,T,T,T,T,T,T,F)
  )

  expect_equal(
    get_parents(ab),
    list(
      .default_id(), #1
      .default_id(), #2
      2,             #3
      .default_id(), #4
      4,             #5
      5,             #6
      4,             #7
      c(3,7),        #8
      8              #9
    )
  )

  expect_equal(
    get_prior(ab),
    list(
      .default_id(), #1
      .default_id(), #2
      .default_id(), #3
      .default_id(), #4
      .default_id(), #5
      .default_id(), #6
      .default_id(), #7
      1,             #8
      .default_id()  #9
    )
  )

  expect_equal(
    get_summary(ab),
    list(
      list(),
      list(),
      list(),
      list(),
      list(),
      list(c(NaN, 4)),
      list(),
      list(),
      list() 
    )
  )

  expect_true(
    is.numeric(get_time(ab)) &&
    is.na(get_time(ab)[8])    # This is the container created by funnel
  )

  expect_equal(
    get_value(ab, warn=FALSE),
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
    get_warnings(ab),
    list(
      .default_warnings(),
      .default_warnings(),
      .default_warnings(),
      .default_warnings(),
      "NaNs produced",
      .default_warnings(),
      .default_warnings(),
      .default_warnings(),
      .default_warnings()
    )
  )

})



test_that("get_* subsetting works", {
  expect_equal(
    get_dependents(ab, index=c(1,8,9)),
    list(
      .default_id(), #1
      9,             #8
      .default_id()  #9
    )
  )

  expect_equal(
    get_code(ab, index=c(1,2)), list('"hi"', '"a"')
  )

  expect_equal(
    get_doc(ab, index=c(3,9)), list("this does stuff", .default_doc())
  )

  expect_equal(
    get_error(ab, index=c(1,9)), list(.default_error(), "die die")
  )

  expect_equal(
    get_id(ab, index=1:3), 1:3
  )

  expect_true(
    is.numeric(get_mem(ab, index=1:2)) &&
    all(get_mem(ab, index=1:2) > 0) &&
    length(get_mem(ab, index=1:2)) == 2
  )

  expect_equal(
    get_meta(ab, index=c(1,3)), list(list(), list(x=1))
  )

  expect_equal(
    get_nest(ab, index=c(1,7)), list(.default_id(), 6)
  )

  expect_equal(
    get_nest_depth(ab, index=c(1,5,9)), c(1,2,1)
  )

  expect_equal(
    get_notes(ab, index=c(1,3,9)),
    list(
      .default_notes(),
      "yolo",
      .default_notes()
    )
  )

  expect_equal(
    get_OK(ab, index=c(1,3,9)), c(T,T,F)
  )

  expect_equal(
    get_parents(ab, index=c(1,3,8)),
    list(
      .default_id(), #1
      2,             #3
      c(3,7)         #8
    )
  )

  expect_equal(
    get_prior(ab, index=c(1,3,8)),
    list(
      .default_id(), #1
      .default_id(), #3
      1              #8
    )
  )

  expect_equal(
    get_summary(ab, index=c(1,6,9)),
    list(
      list(),          #1
      list(c(NaN, 4)), #6
      list()           #9
    )
  )

  expect_true(
    is.numeric(get_time(ab, index=8:9)) &&
    is.na(get_time(ab, index=8:9)[1])    # This is the container created by funnel
  )

  expect_equal(
    get_value(ab, warn=FALSE, index=c(1,5,9)),
    list(
      "hi",       #1
      c(NaN, 16), #5
      list(       #9
        c(NaN, 4),
        "a b"
      )
    )
  )

  expect_equal(
    get_warnings(ab, index=c(1,5,9)),
    list(
      .default_warnings(), #1
      "NaNs produced",     #5
      .default_warnings()  #9
    )
  )

})




test_that("has_* works", {
  expect_equal(
    has_dependents(ab, index=c(1,8,9)),
    c(FALSE, TRUE, FALSE)
  )

  expect_equal(
    has_code(ab, index=c(1,2)), c(TRUE, TRUE)
  )

  expect_equal(
    has_doc(ab, index=c(3,9)), c(TRUE, FALSE)
  )

  expect_equal(
    has_error(ab, index=c(1,9)), c(FALSE, TRUE)
  )

  expect_true(
    all(has_mem(ab)) && length(has_mem(ab)) == 9
  )

  expect_equal(
    has_meta(ab, index=c(1,3)), c(FALSE, TRUE)
  )

  expect_equal(
    has_nest(ab, index=c(1,7)), c(FALSE, TRUE)
  )

  expect_equal(
    has_notes(ab, index=c(1,3,9)),
    c(FALSE,TRUE,FALSE)
  )

  expect_equal(
    has_parents(ab, index=c(1,3,8)),
    c(FALSE, TRUE, TRUE)
  )

  expect_equal(
    has_prior(ab, index=c(1,3,8)),
    c(FALSE, FALSE, TRUE)
  )

  expect_equal(
    has_summary(ab, index=c(1,6,9)),
    c(FALSE, TRUE, FALSE)
  )

  expect_equal(
    has_time(ab, index=8:9), c(FALSE, TRUE)
  )

  expect_equal(
    has_value(ab, index=1:3), c(TRUE, TRUE, FALSE)
  )

  expect_equal(
    has_warnings(ab, index=c(1,5,9)),
    c(FALSE, TRUE, FALSE)
  )
})
