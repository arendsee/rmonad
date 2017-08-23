context("accessor methods")

m <- Rmonad$new()

test_that("The .has_* correctly return false", {
  expect_equal(m$has_branch(),   FALSE)
  expect_equal(m$has_doc(),      FALSE)
  expect_equal(m$has_error(),    FALSE)
  expect_equal(m$has_mem(),      FALSE)
  expect_equal(m$has_notes(),    FALSE)
  expect_equal(m$has_time(),     FALSE)
  expect_equal(m$has_value(),    FALSE)
  expect_equal(m$has_warnings(), FALSE)
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

test_that("Attempting to access a non-existent value should raise an warning", {
  expect_warning({
    x <- 16 %>>% sqrt %>>% sqrt
    m_value(as.list(x)[[1]])
  })
})
