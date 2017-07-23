context("environmental issues")

foo <- function(x) sqrt(x)

test_that("Blocks are built into functions in the correct environment", {
  expect_equal(
    16 %>>% { foo(.) } %>% m_value,
    16 %>>%   foo      %>% m_value
  )
  expect_equal(
    16 %>>% { sqrt(.) } %>% m_value,
    16 %>>%   sqrt      %>% m_value
  )
})
