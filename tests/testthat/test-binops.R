context("binary operators")

test_that('%>>% and esc work (simple)', {
  expect_equal(1 %>>% '*'(2) %>% esc, 2)
  expect_true(1 %>>% '*'(2) %>% m_OK)

  expect_equal(cars %>>% head %>% esc, head(cars))
  expect_true( cars %>>% head %>% m_OK)

  expect_equal(1:3 %>>% '*'(2) %>% esc, c(2,4,6))
  expect_true(1:3 %>>% '*'(2) %>% m_OK)

  expect_error("3" %>>% '*'(2) %>% esc)
})

test_that('%>>% chaining works', {
  expect_equal(1 %>>% '*'(2) %>>% '*'(4) %>>% '*'(3) %>% esc  , 24 )
  expect_true( 1 %>>% '*'(2) %>>% '*'(4) %>>% '*'(3) %>% m_OK      )

  expect_equal(cars %>>% head %>>% colSums %>% esc, colSums(head(cars)))
  expect_true( cars %>>% head %>>% colSums %>% m_OK)

  expect_error(1 %>>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% esc          )
})

test_that('last passing value propagate', {
  expect_equal(1 %>>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% m_value , 2)
  expect_false(1 %>>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% m_OK       )

  expect_equal(iris %>>% head %>>% colSums %>% m_value, head(iris))
  expect_false(iris %>>% head %>>% colSums %>% m_OK)
})

test_that('function passing works with package labels', {
  expect_equal(2 %>>% base::'*'(3) %>>% base::'*'(4) %>% esc, 24)
  expect_true( 2 %>>% base::'*'(3) %>>% base::'*'(4) %>% m_OK   )

  expect_equal(4 %>>% base::sqrt %>% esc, 2)
  expect_true( 4 %>>% base::sqrt %>% m_OK  )

  # NOTE: keep this test
  expect_equal(cars %>>% head %>>% base::as.matrix %>% esc, base::as.matrix(head(cars)))
  expect_true( cars %>>% head %>>% base::as.matrix %>% m_OK)
})

test_that('input storing works', {
  expect_equal(
    256 %v>% sqrt %>>% sqrt %v>% sqrt %>% unstore,
    list(256,NULL,4,2)
  )
})

test_that('parameterization works', {
  expect_equal(c(1,4,2) %>>% order(decreasing=TRUE) %>% esc, c(2,3,1) )
})

test_that('Alteratives (%|>%) work', {
  expect_equal(1:10 %>>% colSums %|>% sum %>% esc, 55)
})

test_that('Alteratives (%||%) work', {
  expect_equal(1 %||% 5 %>>% sqrt %>% uncode, list("1", "sqrt"))
  expect_true(1 %||% 5 %>>% sqrt %>% m_OK)
})

test_that('branching works %>^%', {
  expect_equal(
    16 %>^% sqrt %>^% sqrt %>% unbranch %>% esc %>% lapply(m_value),
    list(16,4,4)
  )
})

diff <- function(a,s) { s - a }
test_that('branching function work %^>%', {
  expect_equal(
    1:10 %>^% '*'(2) %>^% '*'(3) %^>% diff %>% esc,
    -1 * 1:10
  )
  expect_true(
    1:10 %>^% '*'(2) %>^% '*'(3) %^>% diff %>% m_OK,
  )
})

test_that('output toss works %>_%', {
  expect_equal(1 %>_% '*'(3) %>>% '*'(2) %>% uncode, list("1", "*3", "*2"))
  expect_equal(1 %>_% '*'(3) %>>% '*'(2) %>% m_value, 2)
  expect_true( 1 %>_% '*'(3) %>>% '*'(2) %>% m_OK)

  expect_error( 1 %>_% stop(1) %>>% '*'(2) %>% esc)
})

test_that('%*>% safely evaluates failing lists', {
  # works for passing case
  expect_equal( list(3,5,2) %*>% max %>% m_value, 5 )
  # catches error in list
  expect_silent( list(stop(1),5,2) %*>% max )
  # passes on last valid input
  expect_equal( list(stop(1),5,2) %*>% max %>% m_value, list(NULL, 5, 2) )
  # but knows the input is failing
  expect_false( list(stop(1),5,2) %*>% max %>% m_OK )
})

test_that('anonymous expressions can be run', {
  expect_equal( 1:10 %>>% { 4 } %>% esc  , 4 )
  expect_true(  1:10 %>>% { 4 } %>% m_OK     )
})

test_that('dot substitution is performed in anonymous expressions', {
  expect_equal( 1:10 %>>% { . * 2 } %>% esc  , (1:10)*2 )
  expect_true(  1:10 %>>% { . * 2 } %>% m_OK            )

  expect_true(  cars %>>% { sapply(., is.numeric) %>% all } %>% esc  )
  expect_true(  cars %>>% { sapply(., is.numeric) %>% all } %>% m_OK )
  expect_false( iris %>>% { sapply(., is.numeric) %>% all } %>% esc  )
  expect_true(  iris %>>% { sapply(., is.numeric) %>% all } %>% m_OK )
})
