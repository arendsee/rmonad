context("rmonad.R")


test_that('%$>% and esc work', {
  expect_equal(1 %$>% '*'(2) %>% esc, 2)
  expect_equal(1:3 %$>% '*'(2) %>% esc, c(2,4,6))
  expect_error("3" %$>% '*'(2) %>% esc)
})

test_that('%>>% works', {
  expect_equal(1 %$>% '*'(2) %>>% '*'( 4 ) %>>% '*'(3) %>% esc     , 24 )
  expect_error(1 %$>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% esc          )
})

test_that('last passing value propagate', {
  expect_equal(1 %$>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% m_value , 2)
})

test_that('function passing works with package labels', {
  expect_equal(2 %$>% base::'*'(3) %>>% base::'*'(4) %>% esc, 24)
  expect_equal(4 %$>% base::sqrt %>% esc, 2)
})

test_that('parameterization works', {
  expect_equal(c(1,4,2) %$>% order(decreasing=TRUE) %>% esc, c(2,3,1) )
})
