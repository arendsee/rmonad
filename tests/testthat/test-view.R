context("views")

test_that("view of head is identity", {
  m <- as_monad(16) %>% tag('a') %>>% sqrt %>% tag('foo/bar') %v>% sqrt
  expect_true(identical(m, viewID(m, get_id(m, m@head)[[1]])))
})

test_that("view does not change graph", {
  m <- as_monad(16) %>% tag('a') %>>% sqrt %>% tag('foo/bar') %v>% sqrt
  expect_true(identical(
    mtabulate(m), mtabulate(viewID(m, 1))      
  ))
  expect_true(identical(
    mtabulate(m), mtabulate(viewID(m, 2))      
  ))
})


foo <- function(x, pre="yolo"){
  list(x = pre)
  x
}

test_that("view handles failure", {
  # The fail state is propagated. When it is chained with a monadic operator,
  # the fail state propagates and no action is taken, but when it is tagged,
  # the current tag is overwritten.
  a0 <- as_monad("hi") %>>% sqrt %>% tag("a/f") %>>% log %>% tag("b") %>% view("a/f")
  a1 <- as_monad("hi") %>>% sqrt %>% tag("a/f") %>>% log %>% tag("b") %>% view("a/f")
  expect_false(
    a0 %>% get_OK %>% tail(1)
  )
  expect_false(
    a1 %>% get_OK %>% tail(1)
  )
  expect_equal(get_tag(a0) %>% tail(1), list(list(c("a", "f"), "b")))
  expect_equal(get_tag(a1) %>% tail(1), list(list(c("a", "f"), "b")))
})
