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


g1 <- function(g1v1){
  identity(g1v1) %v__% 5
}
test_that("", {
  expect_equal(6 %>>% g1 %>% sapply(m_OK), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(6 %>>% g1 %>% lapply(m_value), list(NULL,6,5,5))
})
