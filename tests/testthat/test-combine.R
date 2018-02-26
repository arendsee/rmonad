context("combine and funnel")

l_non_monadic <- list("hi", 42)
l_monadic <- list(as_monad("hi"), as_monad(42))

test_that('combine fails on non-monadic inputs in list', {
  expect_error(combine(l_non_monadic))
  expect_silent(combine(l_monadic))
  expect_error(combine(5))
})

test_that('funnel stores failing input as node', {
  m <- "hi" %>>% sqrt %>% funnel(x=5) %*>% {. + x}
  # funnel stores NULL in place of the failing input
  expect_equal(get_value(m, size(m))[[1]], list(NULL, x=5))
  # the failing node stores its input
  expect_equal(get_value(m, 3)[[1]], "hi")
})


test_that('combine and funnel store history in list', {

  expect_equal(length(get_id(combine(l_monadic))[-1]), 2)
  expect_equal(length(get_id(funnel("hi", 42))[-1]), 2)

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
  expect_equal(combine(c(as_monad(3), as_monad(5), as_monad(1))) %>% .single_value %>% unlist, c(3, 5, 1))
  expect_equal(funnel(3, 5, 1) %>% .single_value %>% unlist, c(3, 5, 1))

  expect_true(is.list(funnel(3, 5, 1) %>% .single_value))
  expect_true(is.list(combine(list(as_monad(3), as_monad(5), as_monad(1))) %>% .single_value))
})


test_that('combine and funnel work with expressions', {
  expect_equal( combine(list(as_monad(max(3, 2)), as_monad(prod(2, 3)))) %>% .single_value %>% unlist, c(3, 6))
  expect_equal( funnel(               max(3, 2) ,          prod(2, 3))   %>% .single_value %>% unlist, c(3, 6))

  expect_true( combine( list(as_monad(max(3,2)), as_monad(prod(2,3))) ) %>% .single_OK )
  expect_true( funnel(                max(3,2) ,          prod(2,3)   ) %>% .single_OK )
})

test_that('funnel handles errors in expressions', {
  expect_equal(funnel(5, stop(1)) %>% .single_value, list(5, NULL))
  expect_false(funnel(5, stop(1)) %>% .single_OK)
})

test_that('funnel intermediate values are deleted', {
  expect_equal(
    funnel(1:10, 11:20)              %*>%
      (function(x,y){ sum(c(x,y)) }) %>%
      get_value(warn=FALSE),
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

test_that("branches of the same pipeline can be joined", {
  expect_equal(
    {
      256 %v>% sqrt -> m
      m %v>% sqrt %v>% sqrt -> a
      m %v>% sqrt -> b
      funnel(a, b) %*>% sum -> ab
      ab %>% esc
    },
    6
  )
})

test_that("identical steps are joined", {
  expect_equal(
    {
      256 %v>% sqrt -> m
      # The `m %v>% sqrt` step is identical
      m %v>% sqrt %v>% sqrt -> a
      m %v>% sqrt -> b
      funnel(a, b) %*>% sum -> ab
      ab %>% get_value(warn=FALSE)
    },
    list(
      256,
      16,
      4,
      NULL, # 2
      NULL, # funnel
      6     # 4+2
    )
  )
})


test_that("no duplication occurs on pipelines that branch and rejoin", {
  expect_equal(
    {
      256 %v>% sqrt -> m
      m %v>% prod(10) %v>% prod(100) %>% tag('a') -> a
      m %v>% prod(2) %>% tag('b') -> b
      funnel(a, b) %>% tag('ab') %*>% sum -> ab
      ab %>% get_value(warn=FALSE)
    },
    list(
      256,
      16,
      32,
      160,
      16000,
      list(16000, 32),
      16032
    )
  )
})
