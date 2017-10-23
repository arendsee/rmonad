context("accessor methods")

m <- Rmonad()

test_that("The .has_* correctly return false", {
  expect_equal(has_doc(m),      FALSE)
  expect_equal(has_children(m), FALSE)
  expect_equal(has_prior(m),    FALSE)
  expect_equal(has_nest(m),     FALSE)
  expect_equal(has_error(m),    FALSE)
  expect_equal(has_mem(m),      FALSE)
  expect_equal(has_notes(m),    FALSE)
  expect_equal(has_time(m),     FALSE)
  expect_equal(has_value(m),    FALSE)
  expect_equal(has_warnings(m), FALSE)
})

test_that("You get out what you put in", {
  expect_equal({m_OK(m)       <- FALSE;      m_OK(m)       }, FALSE      )
  expect_equal({m_doc(m)      <- "doc";      m_doc(m)      }, "doc"      )
  expect_equal({m_error(m)    <- "error";    m_error(m)    }, "error"    )
  expect_equal({m_notes(m)    <- "notes";    m_notes(m)    }, "notes"    )
  expect_equal({m_value(m)    <- "value";    m_value(m)    }, "value"    )
  expect_equal({m_warnings(m) <- "warnings"; m_warnings(m) }, "warnings" )
})

test_that("You can't put in illegal values", {
  expect_error(m_warnings(.m) <- 34    )
  expect_error(m_notes(.m)    <- FALSE )
})

m1 <- as_monad({warning("w"); message("m"); 46})
m2 <- as_monad({warning("w1"); warning("w2"); message("m1"); message("m2"); 47})
e1 <- as_monad({warning("w"); stop("e"); 48})

test_that("m_* return unlisted results", {
  expect_equal(m_warnings(m1), "w")
  expect_equal(m_warnings(m2), c("w1", "w2"))
  expect_equal(m_notes(m1), "m")
  expect_equal(m_notes(m2), c("m1", "m2"))
  expect_equal(m_error(e1), "e")
})

test_that("ms_* return listed results", {
  expect_equal(ms_warnings(m1)[[1]], "w")
  expect_equal(ms_warnings(m2)[[1]], c("w1", "w2"))
  expect_equal(ms_notes(m1)[[1]],    "m")
  expect_equal(ms_notes(m2)[[1]],    c("m1", "m2"))
  expect_equal(ms_error(e1)[[1]],    "e")
})


test_that("Attempting to access a non-existent value should raise an warning", {
  expect_warning({
    16 %>>% sqrt %>% ms_value
  })
})
