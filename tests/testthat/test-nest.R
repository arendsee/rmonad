context("pipeline nesting")


addit <- function(x,y) x + y

subit <- function(x,y) x - y

foo <- function(x,k=3){
  45 %>>% addit(x) %>>% subit(k)
}

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

bar <- function(x, j=4){
  10 %>>% foo(k=x)
}

test_that("Nesting works for deeply nested functions", {
   expect_equal(20 %>>% bar %>% esc, 35)
   expect_true(20 %>>% bar %>% m_OK)
   expect_equal(
     20 %>>% bar %>% sapply(m_nest_depth),
     c(1,3,3,3,1,2,2,1)
   )
   expect_equal(
     20 %>>% bar %>% lapply(m_value),
     list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,35)
   )
   expect_equal(
     20 %>>% bar %>% sapply(m_code),
     c("10", "45", "addit(x)", "subit(k)", "20", "10", "foo(k = x)", "bar")
   )
})


a_bomb <- function(x,y){
  g <- { x * 2 } %>>% { 7 * . }
  g %>>% { y * . }
}
test_that("Test deep anonymous nesting works", {
   expect_equal(funnel(x=2,y=5) %*>% a_bomb %>% esc, 140)
})
