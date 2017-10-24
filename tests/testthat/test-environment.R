context("environmental issues")

foo <- function(x) sqrt(x)

test_that("Blocks are built into functions in the correct environment", {
  expect_equal(
    16 %>>% { foo(.) } %>% ms_value(warn=FALSE),
    16 %>>%   foo      %>% ms_value(warn=FALSE)
  )
  expect_equal(
    16 %>>% { sqrt(.) } %>% ms_value(warn=FALSE),
    16 %>>%   sqrt      %>% ms_value(warn=FALSE)
  )
})


g1 <- function(g1v1){
  identity(g1v1) %__% 5
}
test_that("%__% within a nest", {
  expect_equal(6 %>>% g1 %>% ms_OK, c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(6 %>>% g1 %>% ms_value(warn=FALSE), list(NULL,6,NULL,5))
})
