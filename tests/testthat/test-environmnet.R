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
g2 <- function(g2v1){
  identity(g2v1) %__% 5
}
test_that("Do %__% and %v__% behave appropriately", {
  expect_equal(6 %>>% g1 %>% sapply(m_OK), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(6 %>>% g1 %>% lapply(m_value), list(NULL,6,NULL,5))

  expect_equal(6 %>>% g2 %>% sapply(m_OK), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(6 %>>% g2 %>% lapply(m_value), list(NULL,NULL,NULL,5))
})
