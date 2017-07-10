context("accessor methods")

m <- new_rmonad()

test_that("The .has_* correctly return false", {
  expect_equal(.has_id(m),       TRUE )
  expect_equal(.has_branch(m),   FALSE)
  expect_equal(.has_doc(m),      FALSE)
  expect_equal(.has_error(m),    FALSE)
  expect_equal(.has_mem(m),      FALSE)
  expect_equal(.has_notes(m),    FALSE)
  expect_equal(.has_time(m),     FALSE)
  expect_equal(.has_value(m),    FALSE)
  expect_equal(.has_warnings(m), FALSE)
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
