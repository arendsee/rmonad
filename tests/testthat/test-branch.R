context("branching")

test_that('simple branching works', {
  expect_equal(
    16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>% unbranch %>% sapply(m_value),
    c(18, 17)
  )
  expect_equal(
    16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>% unbranch %>% sapply(m_OK),
    c(TRUE, TRUE)
  )
  expect_true({
    parent_ids <- 16 %>^% add(1) %>>% add(2) %>% as.list %>%
      lapply(function(x) sapply(m_parents(x), m_id))
    # The first element is the source node, with value 16
    length(parent_ids[[1]]) == 0 &&
    # The two other nodes are its direct children
    parent_ids[[2]] == parent_ids[[3]] &&
    # There should be exactly 3 nodes in the network
    length(parent_ids) == 3
  })
})

test_that('branching works %>^%', {
  expect_equal(
    16 %>^% sqrt %>^% '*'(2) %>% unbranch %>% lapply(esc, warn=FALSE),
    list(16,4,32)
  )
  expect_equal(
    16 %>^% stop(1) %>^% '*'(2) %>% unbranch %>% lapply(esc, warn=FALSE),
    list(16,NULL,32)
  )
})

test_that('Reference sematics do not mess up externally defined rmonads', {
  expect_equal(
    {
      foo_ <- as_monad({ "yolo"; 64 })
      foo_ %>>% {"ggg" ; sqrt(.)} # The externally defined 'foo_' should not be modified
      foo_ %>>% {"hhh" ; sqrt(.)} %>% esc
    },
    8
  )
})
