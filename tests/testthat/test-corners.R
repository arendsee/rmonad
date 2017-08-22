context("corner cases")

test_that("empty anonymouse functions", {

  # This breaks when you mess with the order of the extract_docstrings function
  expect_equal(16 %>>% (function(x){}) %>% esc, NULL)
  expect_equal(as_monad({}) %>% esc, NULL)

  # This just can never work, because bind wants to pass a value
  # It might be preferable to catch this and throw a special error
  expect_false(16 %>>% (function(){}) %>% m_OK)

})

test_that("NULL can be value", {
  expect_equal(
    NULL %>>% as.data.frame %>% esc,
    as.data.frame(NULL)
  )
})

test_that("Error messages can be empty", {
  expect_true(as_monad(stop()) %>% has_error)            
})
