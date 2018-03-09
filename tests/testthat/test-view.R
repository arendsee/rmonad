context("views")

m <- as_monad(16) %>% tag('a') %>>% sqrt %>% tag('foo/bar') %v>% sqrt

test_that("view of head is identity", {
  expect_true(identical(m, viewID(m, get_id(m, m@head)[[1]])))
})

test_that("view does not change graph", {
  expect_true(identical(
    mtabulate(m), mtabulate(viewID(m, 1))      
  ))
  expect_true(identical(
    mtabulate(m), mtabulate(viewID(m, 2))      
  ))
})
