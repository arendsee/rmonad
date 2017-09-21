context("github issues")

test_that('issue #1: "%__%" does not double nest lhs', {
  expect_silent(5 %__% as_monad(4) %>>% sqrt)
  expect_equal(5 %__% as_monad(4) %>>% sqrt %>% m_value, 2)
})

test_that('issue #2: funnel works with %__%', {
  expect_equal( funnel(16 %v>% sqrt) %>% lapply(m_value), list(16, 4, list(4)))
  expect_equal( "hi" %__% funnel(16 %v>% sqrt) %>% lapply(m_value), list("hi", 16, 4, list(4))) 
})

test_that('issue #3: nested errors are localized', {
  # The failing node should hold the final value 
  expect_equal(as.list("hi" %>>% { . %>>% log })[[3]] %>% m_value, "hi")
  # The node containing the failing nested pipeline should hold nothing 
  expect_warning(as.list("hi" %>>% { . %>>% log })[[4]] %>% m_value)
  # Can get intermediate values from inside the nest
  expect_warning(as.list("hi" %>>% { . %>>% paste("bi") %>>% log })[[4]] %>% m_value, "bi")
  expect_false(as.list("hi" %>>% { . %>>% paste("bi") %>>% log })[[4]] %>% m_OK)
})
