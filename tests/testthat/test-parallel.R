context("higher-order functions")

f <- function(x, a){ x * a }
foo <- function(x) { x %>>% f(3) %>% tag(as.character(x)) }
bar <- function(x, a) { x %>>% f(a) %>% tag(as.character(x)) }
mayfail <- function(x) { x %>>% log %>>% sqrt %>% tag(as.character(x)) }
test_that("basic loop", {

  # The correct output is obtained
  expect_equal(
    c(1, 2, 3) %v>% f(2) %>% loop(foo) %>>% lapply(f, 4) %>% esc,
    list(24,48,72)
  )

  # History is preserved across the loop
  expect_equal(
    c(1, 2) %v>% f(2) %>% loop(foo) %>>% lapply(f, 4) %>%
      get_value(1) %>% unlist,
    c(1, 2)
  )

  # Topology is correct
  expect_equal(
    c(1, 2) %v>% f(2) %>% loop(foo) %>>% lapply(f, 4) %>% get_parents,
    list(
      integer(0),
      1,
      integer(0),
      3,
      integer(0),
      5,
      c(2,4,6),
      7
    )
  )

  # Parameterized pipelines work
  expect_equal(
    c(1, 2, 3) %v>% f(2) %>% loop(bar, a=3) %>>% lapply(f, 4) %>% esc,
    list(24,48,72)
  )

  # Failures are detected
  expect_equal(
    as_monad(list(2, "bowzer")) %>% loop(mayfail) %>% mtabulate %>% {.$OK},
    c(T,T,F,T,T,T,F)
  )
})
