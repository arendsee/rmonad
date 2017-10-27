context("branching")

test_that('simple branching works', {
  expect_equal(
    16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>%
      {ms_value(.)[unlist(ms_dependents(.))]},
    list(17, 18)
  )
  expect_equal(
    16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>% ms_OK,
    c(TRUE, TRUE, TRUE)
  )
  expect_true({
    parent_ids <- 16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>% ms_parents 
    # The first element is the source node, with value 16
    length(parent_ids[[1]]) == 0 &&
    # The two other nodes are its direct children
    parent_ids[[2]] == 1 && parent_ids[[3]] == 1 &&
    # There should be exactly 3 nodes in the network
    length(parent_ids) == 3
  })
})

test_that('branching works %>^%', {
  expect_equal(
    16 %>^% sqrt %>^% '*'(2) %>% ms_value,
    list(16,4,32)
  )
  expect_equal(
    16 %>^% stop(1) %>^% '*'(2) %>% ms_value(warn=FALSE), 
    list(16,NULL,32)
  )
})

test_that('New rmonad dont overwrite old ones (not an issue without R6)', {
  expect_equal(
    {
      foo_ <- as_monad({ "yolo"; 64 })
      foo_ %>>% {"ggg" ; sqrt(.)} # The externally defined 'foo_' should not be modified
      foo_ %>>% {"hhh" ; sqrt(.)} %>% esc
    },
    8
  )
})
