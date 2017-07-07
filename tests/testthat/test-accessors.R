context("accessors")

m <- Rmonad()

# TODO: complete the test coverage for the accessors

test_that("The .has_* correctly return false", {
  expect_equal(.has_doc(m),      FALSE)
  expect_equal(.has_error(m),    FALSE)
  expect_equal(.has_warnings(m), FALSE)
  expect_equal(.has_notes(m),    FALSE)
  expect_equal(.has_branch(m),   FALSE)
  expect_equal(.has_value(m),    FALSE)
})

test_that("You get out what you put in", {
  expect_equal({ .m <- m; m_doc(.m)      <- "doc";            m_doc(.m)      }, "doc")
  expect_equal({ .m <- m; m_error(.m)    <- "error";          m_error(.m)    }, "error")
  expect_equal({ .m <- m; m_value(.m)    <- list("value");    m_value(.m)    }, list("value"))
  expect_equal({ .m <- m; m_warnings(.m) <- list("warnings"); m_warnings(.m) }, list("warnings"))
  expect_equal({ .m <- m; m_notes(.m)    <- list("notes");    m_notes(.m)    }, list("notes"))
})


test_that("You can't put in illegal values", {
  expect_error(m_warnings(.m) <- "warning")
  expect_error(m_notes(.m)    <- "notes")
})
