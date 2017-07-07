context("monadic_eval.R")

l <- list("hi", 42)
test_that('combine works with lists given as variables', {
  expect_equal( combine(l) %>% m_value, l)
  expect_true(  combine(l) %>% m_OK)
})

test_that('combine and lsmeval store history in list', {

  expect_equal( length(m_history(combine(l))),                2 )
  expect_equal( length(m_history(lsmeval(list("hi", 42)))), 2 )

})

test_that('combine and lsmeval store all results into an order preserving list', {
  expect_equal( combine(c(3,5,1))    %>% m_value %>% unlist, c(3,5,1) )
  expect_equal( lsmeval(list(3,5,1)) %>% m_value %>% unlist, c(3,5,1) )

  expect_true( is.list(lsmeval(list(3,5,1)) %>% m_value) )
  expect_true( is.list(combine(list(3,5,1)) %>% m_value) )
})


test_that('combine and lsmeval work with expressions', {
  expect_equal( combine( list(max(3,2), prod(2,3)) ) %>% m_value %>% unlist, c(3,6) )
  expect_equal( lsmeval( list(max(3,2), prod(2,3)) ) %>% m_value %>% unlist, c(3,6) )

  expect_true( combine( list(max(3,2), prod(2,3)) ) %>% m_OK )
  expect_true( lsmeval( list(max(3,2), prod(2,3)) ) %>% m_OK )
})

test_that('lsmeval handles errors in expressions', {
  expect_equal( lsmeval( list(5, stop(1)) ) %>% m_value, list(5, NULL) )
  expect_false( lsmeval( list(5, stop(1)) ) %>% m_OK )
})
