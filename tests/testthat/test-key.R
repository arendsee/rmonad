context("key handling")

test_that("The key in funnel is dependent on order", {
  expect_true(
    !identical(
      funnel("foo", "bar") %>% get_key,
      funnel("bar", "foo") %>% get_key
    )
  ) 
})

test_that("Key is dependent on the path", {
  expect_true(
    all(
      (2 %>>% prod(3) %>>% prod(2) %>% get_key)[[3]] !=
      (2 %>>% prod(2) %>>% prod(3) %>% get_key)[[3]]
    )
  )
})
