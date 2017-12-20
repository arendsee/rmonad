context("branching")

test_that('simple branching works', {
  expect_equal(
    16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>%
      {get_value(.)[unlist(get_dependents(.))]},
    list(17, 18)
  )
  expect_equal(
    16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>% get_OK,
    c(TRUE, TRUE, TRUE)
  )
  expect_true({
    parent_ids <- 16 %>^% magrittr::add(1) %>>% magrittr::add(2) %>% get_parents 
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
    16 %>^% sqrt %>^% '*'(2) %>% get_value,
    list(16,4,32)
  )
  expect_equal(
    16 %>^% stop(1) %>^% '*'(2) %>% get_value(warn=FALSE), 
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


# -----------------------------------------------------------------------------
m <- 'a' %v>% paste0('b') %>^%
{
  paste0(., '-c') %v>%
  paste0(   'd')  %v>%
  paste0(   'e-')
} %v>%
paste0('f') %v>% paste0('g') %v>% paste0('h')
# value   | depth | vid | pid | tid | nid | notes
# a       | 1     |  1  |  -  |  -  |  -  |
# ab      | 1     |  2  |  1  |  -  |  -  |
# ab-c    | 2     |  3  |  -  |  2  |  -  | input to nest
# ab-cd   | 2     |  4  |  3  |  -  |  -  |
# NULL,   | 2     |  5  |  4  |  -  |  -  | output of nest (value sent to wrapper)
# ab-cde- | 1     |  6  |  2  |  -  |  5  | the nest wrapper (holds output of nest)
# abf     | 1     |  7  |  2  |  -  |  -  | first element of second branch
# abfg    | 1     |  8  |  7  |  -  |  -  |
# abfgh   | 1     |  9  |  8  |  -  |  -  |
# vid - vertex id
# did - dependent id
# tid - transitive id
# nid - nest id
# =============================================================================
test_that('Branching into nests correctly makes transitive links', {
  expect_equal(get_value(m,F), list("a", "ab", "ab-c", "ab-cd", NULL, "ab-cde-", "abf", "abfg", "abfgh"))
  expect_equal(get_dependents(m), list(2, c(6,7), 4, 5, integer(0), integer(0), 8, 9, integer(0)))
  expect_equal(get_parents(m), list(integer(0), 1, 2, 3, 4, 2, 2, 7, 8))
})
