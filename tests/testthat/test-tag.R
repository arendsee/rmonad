context("tags and views")

test_that("tag sets tag", {
  # can set tag from as_monad
  expect_equal(as_monad(5, tag='a') %>% get_tag, list("a"))
  # default tag values are empty strings
  expect_equal(
    as_monad(16) %>>% sqrt %>>% sqrt %>% get_tag,
    list("", "", "")
  )
  # tag sets the head node
  expect_equal(
    as_monad(16) %>% tag('a') %>>% sqrt %>% tag('b') %>>% sqrt %>% get_tag,
    list("a", "b", "")
  )
})

m <- as_monad(16) %>% tag('a') %v>% sqrt %>% tag('b') %v>% sqrt
test_that("view gets tagged node", {
  expect_equal(view(m, 'b') %>% esc, 4)
})

test_that("has_tag works", {
  expect_equal(has_tag(m), c(TRUE, TRUE, FALSE))
})

m2 <- funnel(view(m, 'a'), view(m, 'b'), 99) %*>% sum
test_that("view works in funnel", {
  expect_equal(esc(m2), 119)
  expect_equal(get_dependents(m2),    list(5L,   c(3L,5L), c(4L,5L), integer(0), 6L,   integer(0)))
  expect_equal(get_value(m2, warn=F), list(NULL, NULL,     4,        2,          NULL, 119       ))
})

f <- make_recacher(memory_cache)
m3 <- 'a' %>>% paste('b') %>% f(c('a', 'b')) %>>%
               paste('c') %>% f(c('a', 'c')) %>>%
               paste('d') %>% f('foo') %>>%
               paste('e')
test_that("multi level tags work", {
  expect_equal(get_value(m3, tag='a'), list(c('a','b'), c('a','b','c')))
  expect_equal(get_value(m3, tag=c('a', 'b')), list(c('a','b')))
})
