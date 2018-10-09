context("tags and views")

m1 <- as_monad(16) %>% tag('a') %>>% sqrt %>% tag('foo/bar') %v>% sqrt
m2 <- funnel(view(m1, 'a'), view(m1, 'foo/bar'), 99) %*>% sum
m3 <- 'a' %>>% paste('b') %>% tag(c('foo', 'bar')) %>>%
               paste('c') %>% tag(c('foo', 'rad')) %>>%
               paste('d') %>% tag('baz') %>>%
               paste('e')

test_that("tag utilities work", {
  expect_equal(.match_tag(m3, c("foo", "bar")), 2)
  expect_equal(.match_tag(m3, "foo/bar"), 2)
  expect_equal(.named_match_tag(m3, c("foo", "bar")),
               list(indices=2L, names="foo/bar"))
  expect_equal(.named_match_tag(m3, "foo/bar"),
               list(indices=2L, names="foo/bar"))
})

test_that("tag sets tag", {
  # can set tag from as_monad
  expect_equal(as_monad(5, tag='a') %>% get_tag, list(list("a")))
  # tag splits on '/'
  expect_equal(as_monad(5, tag='foo/bar') %>% get_tag, list(list(c("foo", "bar"))))
  # default tag values are empty lists
  expect_equal(
    as_monad(16) %>>% sqrt %>>% sqrt %>% get_tag,
    list(list(), list(), list())
  )
  # tag sets the head node
  expect_equal(
    as_monad(16) %>% tag('a') %>>% sqrt %>% tag('b') %>>% sqrt %>% get_tag,
    list(list("a"), list("b"), list())
  )
  # tag sets the head node
  expect_equal(
    as_monad(16) %>% tag('a', 'foo') %>>% sqrt %>% tag('b') %>>% sqrt %>% get_tag,
    list(list(c("a", "foo")), list("b"), list())
  )
})

test_that("view gets tagged node", {
  expect_equal(view(m1, 'a') %>% esc, 16)
  expect_equal(view(m1, 'foo/bar') %>% esc, 4)
  expect_equal(view(m1, c('foo','bar')) %>% esc, 4)
})


test_that("has_tag works", {
  expect_equal(has_tag(m1), c(TRUE, TRUE, FALSE))
})

test_that("view works in funnel", {
  expect_equal(esc(m2), 119)
  expect_equal(get_dependents(m2),    list(5L,   c(3L,5L), c(4L,5L), integer(0), 6L,   integer(0)))
  expect_equal(get_value(m2, warn=F), list(NULL, 16,       4,        2,          NULL, 119       ))
})

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
m4 <- 256 %>% f(tag=c('a', 'b')) %>>%
      sqrt %>% f(tag='b') %>>%
      sqrt %>% f(tag=c('a', 'b'))
m5 <- 256 %>% f('a') %>>%
      sqrt %>% f('b') %>>%
      sqrt %>% f('a')
test_that("Can search nested tags", {
  # TODO: should this number the tags, creating output of ("a/b/1", "a/b/2")?
  expect_equal(names(get_value(m4, tag=c('a', 'b'))), c("a/b", "a/b"))
  expect_equal(names(get_value(m5, tag='a')), c("a", "a"))
})

f <- make_recacher(memory_cache)
m6 <- 256 %>% f('a') %>>%
  sqrt %>% f('b') %>% funnel(a = ., b=view(., 'a')) %*>%
  sum
test_that("Cached tags are preserved", {
  expect_equal(get_tag(m6), list(list("a"), list("b"), list(), list()))
  expect_equal(get_value(m6, warn=F), list(256, 16, NULL, 272))
})
