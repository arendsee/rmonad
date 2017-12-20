context("accessor methods")

m <- as_monad(42)

test_that("The .has_* correctly return false", {
  expect_equal(has_doc(m),        FALSE)
  expect_equal(has_dependents(m), FALSE)
  expect_equal(has_prior(m),      FALSE)
  expect_equal(has_nest(m),       FALSE)
  expect_equal(has_error(m),      FALSE)
  expect_equal(has_mem(m),        TRUE )
  expect_equal(has_notes(m),      FALSE)
  expect_equal(has_time(m),       TRUE )
  expect_equal(has_value(m),      TRUE )
  expect_equal(has_warnings(m),   FALSE)
})

test_that("You get out what you put in", {
  expect_equal({.single_OK(m)       <- FALSE;      .single_OK(m)       }, FALSE      )
  expect_equal({.single_doc(m)      <- "doc";      .single_doc(m)      }, "doc"      )
  expect_equal({.single_error(m)    <- "error";    .single_error(m)    }, "error"    )
  expect_equal({.single_notes(m)    <- "notes";    .single_notes(m)    }, "notes"    )
  expect_equal({.single_value(m)    <- "value";    .single_value(m)    }, "value"    )
  expect_equal({.single_warnings(m) <- "warnings"; .single_warnings(m) }, "warnings" )
})

test_that("You can't put in illegal values", {
  expect_error(.single_warnings(.m) <- 34    )
  expect_error(.single_notes(.m)    <- FALSE )
})

m1 <- as_monad({warning("w"); message("m"); 46})
m2 <- as_monad({warning("w1"); warning("w2"); message("m1"); message("m2"); 47})
e1 <- as_monad({warning("w"); stop("e"); 48})

test_that(".single_* return unlisted results", {
  expect_equal(.single_warnings(m1), "w")
  expect_equal(.single_warnings(m2), c("w1", "w2"))
  ## TODO: these tests, and all others that use notes, fail due to a bug in
  ## testthat (see issue #693). They do pass in the old version of testthat.
  ## For now I will comment out the tests.
  # expect_equal(.single_notes(m1), "m")
  # expect_equal(.single_notes(m2), c("m1", "m2"))
  expect_equal(.single_error(e1), "e")
})

test_that("get_* return listed results", {
  expect_equal(get_warnings(m1)[[1]], "w")
  expect_equal(get_warnings(m2)[[1]], c("w1", "w2"))
  ## TODO: reinstate when testthat is fixed
  # expect_equal(get_notes(m1)[[1]],    "m")
  # expect_equal(get_notes(m2)[[1]],    c("m1", "m2"))
  expect_equal(get_error(e1)[[1]],    "e")
})


test_that("Attempting to access a non-existent value should raise an warning", {
  expect_warning({
    16 %>>% sqrt %>% get_value
  })
})
