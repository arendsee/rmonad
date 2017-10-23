context("pipeline nesting")

addit <- function(x,y) x + y

subit <- function(x,y) x - y

foo <- function(x,k=3){
  45 %>>% addit(x) %>>% subit(k)
}

bar <- function(x, j=4){
  10 %>>% foo(k=x)
}

test_that("Nesting works for named functions", {
  expect_equal(4 %>>% foo %>% esc, 46)
  expect_equal(
    4 %>>% foo %>% ms_code %>% unlist,
    c("4", "45", "addit(x)", "subit(k)", "foo")
  )
  expect_equal(
    4 %>>% foo %>% ms_parents %>% sapply(length),
    c(0,0,1,1,1)
  )
  expect_equal(
    4 %>>% foo %>% ms_nest %>% sapply(length),
    c(0,0,0,0,1)
  )
})

test_that("Nesting works for deeply nested functions", {
   expect_equal(20 %>>% bar %>% esc, 35)
   expect_true(20 %>>% bar %>% m_OK)
   expect_equal(
     20 %>>% bar %>% ms_nest_depth,
     c(1,2,3,3,3,2,1)
   )
   expect_equal(
     20 %>>% bar %>% ms_value(warn=FALSE),
     list(NULL,NULL,NULL,NULL,NULL,NULL,35)
   )
   expect_equal(
     20 %>>% bar %>% ms_code %>% unlist,
     c("20", "10", "45", "addit(x)", "subit(k)", "foo(k = x)", "bar")
   )
})



a_bomb <- function(x,y){
  g <- { x * 2 } %>>% { 7 * . }
  g %>>% { y * . }
}
test_that("Test deep anonymous nesting", {
   expect_equal(funnel(x=2,y=5) %*>% a_bomb %>% esc, 140)
})



h_bomb <- function(x){
  # This catches a past bug in declaration evaluation
  # DO NOT refactor
  g <- subset(x, cyl > 4)
  g %>>% max
}
test_that("Nothing explodes when NSE is used in nested declarations", {
   expect_equal(
     funnel(x=mtcars) %*>% h_bomb %>% m_value,
     h_bomb(mtcars) %>% m_value
  )
  expect_true(funnel(x=mtcars) %*>% h_bomb %>% m_OK)
})
