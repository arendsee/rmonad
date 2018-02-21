context("combine and funnel")

foo <- function(x) { x %>>% sqrt %>% tag(as.character(x)) }
test_that("basic loop", {
  # The correct output is obtained
  expect_equal(
    c(256, 6561) %v>% sqrt %>% loop(foo) %>>% lapply(sqrt) %>% esc,
    list(2,3)
  )
  # History is preserved across the loop
  expect_equal(
    c(256, 6561) %v>% sqrt %>% loop(foo) %>>% lapply(sqrt) %>%
      get_value(1) %>% unlist,
    c(256, 6561)
  )
  # Topology is correct
  expect_equal(
    c(256, 6561) %v>% sqrt %>% loop(foo) %>>% lapply(sqrt) %>% get_parents,
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
})
