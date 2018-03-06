context("tags and views")

test_that("tag sets tag", {
  # can set tag from as_monad
  expect_equal(as_monad(5, tag='a') %>% get_tag, list("a"))
  # tag splits on '/'
  expect_equal(as_monad(5, tag='foo/bar') %>% get_tag, list(c("foo", "bar")))
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
  # tag sets the head node
  expect_equal(
    as_monad(16) %>% tag('a', 'foo') %>>% sqrt %>% tag('b') %>>% sqrt %>% get_tag,
    list(c("a", "foo"), "b", "")
  )
})

m <- as_monad(16) %>% tag('a') %>>% sqrt %>% tag('foo/bar') %v>% sqrt
test_that("view gets tagged node", {
  expect_equal(view(m, 'a') %>% esc, 16)
  expect_equal(view(m, 'foo/bar') %>% esc, 4)
  expect_equal(view(m, c('foo','bar')) %>% esc, 4)
})


test_that("has_tag works", {
  expect_equal(has_tag(m), c(TRUE, TRUE, FALSE))
})

m2 <- funnel(view(m, 'a'), view(m, 'foo/bar'), 99) %*>% sum
test_that("view works in funnel", {
  expect_equal(esc(m2), 119)
  expect_equal(get_dependents(m2),    list(5L,   c(3L,5L), c(4L,5L), integer(0), 6L,   integer(0)))
  expect_equal(get_value(m2, warn=F), list(NULL, 16,       4,        2,          NULL, 119       ))
})

m3 <- 'a' %>>% paste('b') %>% tag(c('foo', 'bar')) %>>%
               paste('c') %>% tag(c('foo', 'rad')) %>>%
               paste('d') %>% tag('baz') %>>%
               paste('e')
test_that("multi level tags work", {
  expect_equal(get_value(m3, tag='foo'), list('foo/bar'='a b', 'foo/rad'='a b c'))
  expect_equal(get_value(m3, tag=c('foo', 'rad')), list('foo/rad'=c('a b c')))
})

test_that("other get_* accessors work", {
  expect_equal(get_OK(m3, tag='foo'), c(T,T))
  expect_equal(get_OK(m3, tag='foo/bar'), T)
  # I could check the actual return values, but that would probably be
  # overkill. So I just ensure nothing explodes. 
  expect_equal(get_code       (m3, tag='foo') %>% length, 2 )
  expect_equal(get_dependents (m3, tag='foo') %>% length, 2 )
  expect_equal(get_doc        (m3, tag='foo') %>% length, 2 )
  expect_equal(get_error      (m3, tag='foo') %>% length, 2 )
  expect_equal(get_id         (m3, tag='foo') %>% length, 2 )
  expect_equal(get_mem        (m3, tag='foo') %>% length, 2 )
  expect_equal(get_meta       (m3, tag='foo') %>% length, 2 )
  expect_equal(get_nest       (m3, tag='foo') %>% length, 2 )
  expect_equal(get_nest_depth (m3, tag='foo') %>% length, 2 )
  expect_equal(get_notes      (m3, tag='foo') %>% length, 2 )
  expect_equal(get_parents    (m3, tag='foo') %>% length, 2 )
  expect_equal(get_prior      (m3, tag='foo') %>% length, 2 )
  expect_equal(get_summary    (m3, tag='foo') %>% length, 2 )
  expect_equal(get_tag        (m3, tag='foo') %>% length, 2 )
  expect_equal(get_time       (m3, tag='foo') %>% length, 2 )
  expect_equal(get_value      (m3, tag='foo') %>% length, 2 )
})

f <- make_recacher(memory_cache)
m4 <- 256 %>% f(c('a', 'b')) %>>% sqrt %>% f('b') %>>% sqrt %>% f(c('a', 'b'))
m5 <- 256 %>% f('a') %>>% sqrt %>% f('b') %>>% sqrt %>% f('a')
test_that("Can search nested tags", {
  expect_equal(names(get_value(m4, tag=c('a', 'b'))), c("a/b/1", "a/b/3"))
  expect_equal(names(get_value(m5, tag='a')), c("a/1", "a/3"))
})

f <- make_recacher(memory_cache)
m6 <- 256 %>% f('a') %>>%
  sqrt %>% f('b') %>% funnel(a = ., b=view(., 'a')) %*>%
  sum
test_that("Cached tags are preserved", {
  expect_equal(get_tag(m6), list("a", "b", "", ""))
  expect_equal(get_value(m6, warn=F), list(256, 16, NULL, 272))
})
