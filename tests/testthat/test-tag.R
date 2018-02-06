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
m3 <- 'a' %>>% paste('b') %>% f(c('#1', '#1')) %>>%
               paste('c') %>% f(c('#1', '#2')) %>>%
               paste('d') %>% f('#3') %>>%
               paste('e')
test_that("multi level tags work", {
  expect_equal(get_value(m3, tag='#1'), list('a b', 'a b c'))
  expect_equal(get_value(m3, tag=c('#1', '#2')), list(c('a b c')))
})

test_that("other get_* accessors work", {
  expect_equal(get_OK(m3, tag='#1'), c(T,T))
  # I could check the actual return values, but that would probably be
  # overkill. So I just ensure nothing explodes. 
  expect_equal(get_code       (m3, tag='#1') %>% length, 2 )
  expect_equal(get_dependents (m3, tag='#1') %>% length, 2 )
  expect_equal(get_doc        (m3, tag='#1') %>% length, 2 )
  expect_equal(get_error      (m3, tag='#1') %>% length, 2 )
  expect_equal(get_id         (m3, tag='#1') %>% length, 2 )
  expect_equal(get_mem        (m3, tag='#1') %>% length, 2 )
  expect_equal(get_meta       (m3, tag='#1') %>% length, 2 )
  expect_equal(get_nest       (m3, tag='#1') %>% length, 2 )
  expect_equal(get_nest_depth (m3, tag='#1') %>% length, 2 )
  expect_equal(get_notes      (m3, tag='#1') %>% length, 2 )
  expect_equal(get_parents    (m3, tag='#1') %>% length, 2 )
  expect_equal(get_prior      (m3, tag='#1') %>% length, 2 )
  expect_equal(get_summary    (m3, tag='#1') %>% length, 2 )
  expect_equal(get_tag        (m3, tag='#1') %>% length, 2 )
  expect_equal(get_time       (m3, tag='#1') %>% length, 2 )
  expect_equal(get_value      (m3, tag='#1') %>% length, 2 )
})
