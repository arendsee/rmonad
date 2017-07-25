context("pipeline nesting")


addit <- function(x,y) x + y

subit <- function(x,y) x - y

foo <- function(x,k=3){
  45 %>>% addit(x) %>>% subit(k)
}

# TODO: add tests after nesting is implemented again
test_that("Nesting works for named functions", {
  expect_equal(4 %>>% foo %>% esc, 46)
  expect_equal(
    4 %>>% foo %>% sapply(m_code),
    c("4", "45", "addit(x)", "subit(k)", "foo")
  )
  expect_equal(
    4 %>>% foo %>% lapply(m_parents) %>% sapply(length),
    c(0,0,2,1,1)
  )
})
