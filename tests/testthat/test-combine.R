context("combine and funnel")

l_non_monadic <- list("hi", 42)
l_monadic <- list(as_monad("hi"), as_monad(42))

test_that('combine fails on non-monadic inputs in list', {
  expect_error(combine(l_non_monadic))
  expect_silent(combine(l_monadic))
  expect_error(combine(5))
})

test_that('combine and funnel store history in list', {

  expect_equal(length(ms_id(combine(l_monadic))[-1]), 2)
  expect_equal(length(ms_id(funnel("hi", 42))[-1]), 2)

  # basic anonymous functions work
  expect_equal({
    "a" %>%
       funnel(b="b", c="c") %*>%
       (function(a,b,c) {paste(a,b,c)}) %>% esc 
  }, "a b c" )

  # keyword arguments are respected
  expect_equal({
    "a" %>%
       funnel(c="c", b="b") %*>%
       (function(a,b,c) {paste(a,b,c)}) %>% esc 
  }, "a b c" )

  # The value from the pipe defaults to first position
  expect_equal({
    "a" %>%
       funnel("c", "b") %*>%
       (function(a,b,c) {paste(a,b,c)}) %>% esc 
  }, "a c b" )

})

test_that('combine and funnel store all results into an order preserving list', {
  expect_equal(combine(c(as_monad(3), as_monad(5), as_monad(1))) %>% m_value %>% unlist, c(3, 5, 1))
  expect_equal(funnel(3, 5, 1) %>% m_value %>% unlist, c(3, 5, 1))

  expect_true(is.list(funnel(3, 5, 1) %>% m_value))
  expect_true(is.list(combine(list(as_monad(3), as_monad(5), as_monad(1))) %>% m_value))
})


test_that('combine and funnel work with expressions', {
  expect_equal( combine(list(as_monad(max(3, 2)), as_monad(prod(2, 3)))) %>% m_value %>% unlist, c(3, 6))
  expect_equal( funnel(               max(3, 2) ,          prod(2, 3))   %>% m_value %>% unlist, c(3, 6))

  expect_true( combine( list(as_monad(max(3,2)), as_monad(prod(2,3))) ) %>% m_OK )
  expect_true( funnel(                max(3,2) ,          prod(2,3)   ) %>% m_OK )
})

test_that('funnel handles errors in expressions', {
  expect_equal(funnel(5, stop(1)) %>% m_value, list(5, NULL))
  expect_false(funnel(5, stop(1)) %>% m_OK)
})

test_that('funnel intermediate values are deleted', {
  expect_equal(
    funnel(1:10, 11:20)              %*>%
      (function(x,y){ sum(c(x,y)) }) %>%
      ms_value(warn=FALSE),
    list(NULL, NULL, NULL, 210)
  )
})

test_that('funnel does not alter the monads it uses', {
  # This will break if `funnel` doesn't properly clone the parents before
  # removing their values
  expect_equal(
    {
      foo_ <- 36 %>% sqrt
      funnel(foo_, 7) %*>%
        (function(x,y){x+y}) %>%
        funnel(foo_) %*>%
        (function(x,y){x+y}) %>% esc
    },
    19
  )
})
