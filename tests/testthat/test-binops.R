context("binary operators")

test_that('%>>% and esc work (simple)', {
  expect_equal(1 %>>% '*'(2) %>% esc, 2)
  expect_true(1 %>>% '*'(2) %>% .single_OK)

  expect_equal(cars %>>% head %>% esc, head(cars))
  expect_true( cars %>>% head %>% .single_OK)

  expect_equal(1:3 %>>% '*'(2) %>% esc, c(2,4,6))
  expect_true( 1:3 %>>% '*'(2) %>% .single_OK)

  expect_error("3" %>>% '*'(2) %>% esc)

  expect_equal(1:3 %>>% (function(x){x^2}) %>>% '*'(2) %>% esc, 2*((1:3)^2) )
  expect_equal(1:3 %>>% (function(x){x^2})             %>% esc,   ((1:3)^2) )
})

test_that('%>>% chaining works', {
  expect_equal(1 %>>% '*'(2) %>>% '*'(4) %>>% '*'(3) %>% esc  , 24 )
  expect_true( 1 %>>% '*'(2) %>>% '*'(4) %>>% '*'(3) %>% .single_OK      )

  expect_equal(cars %>>% head %>>% colSums %>% esc, colSums(head(cars)))
  expect_true( cars %>>% head %>>% colSums %>% .single_OK)

  expect_error(1 %>>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% esc          )
})

test_that('last passing value propagate', {
  expect_equal(1 %>>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% .single_value , 2)
  expect_false(1 %>>% '*'(2) %>>% '*'('4') %>>% '*'(3) %>% .single_OK       )

  expect_equal(iris %>>% head %>>% colSums %>% .single_value, head(iris))
  expect_false(iris %>>% head %>>% colSums %>% .single_OK)
})

test_that('function passing works with package labels', {
  expect_equal(2 %>>% base::'*'(3) %>>% base::'*'(4) %>% esc, 24)
  expect_true( 2 %>>% base::'*'(3) %>>% base::'*'(4) %>% .single_OK   )

  expect_equal(4 %>>% base::sqrt %>% esc, 2)
  expect_true( 4 %>>% base::sqrt %>% .single_OK  )

  # NOTE: keep this test
  expect_equal(cars %>>% head %>>% base::as.matrix %>% esc, base::as.matrix(head(cars)))
  expect_true( cars %>>% head %>>% base::as.matrix %>% .single_OK)
})

test_that('input storing works', {
  expect_equal(
    256 %v>% sqrt %>>% sqrt %v>% sqrt %>% get_value(warn=FALSE),
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
  expect_equal(1 %||% 5 %>>% sqrt %>% get_code, list("1", "sqrt"))
  expect_true(1 %||% 5 %>>% sqrt %>% .single_OK)
})

test_that('output toss works %>_%', {
  expect_equal(1 %>_% '*'(3) %>>% '*'(2) %>% get_code, list("1", "*3", "*2"))
  expect_equal(1 %>_% '*'(3) %>>% '*'(2) %>% .single_value, 2)
  expect_true( 1 %>_% '*'(3) %>>% '*'(2) %>% .single_OK)

  expect_error( 1 %>_% stop(1) %>>% '*'(2) %>% esc)
})

test_that('%*>% safely evaluates failing lists', {
  # works for passing case
  expect_equal( list(3,5,2) %*>% max %>% .single_value, 5 )
  # catches error in list
  expect_silent( list(stop(1),5,2) %*>% max )
  # passes on last valid input
  expect_equal( list(stop(1),5,2) %*>% max %>% .single_value, list(NULL, 5, 2) )
  # but knows the input is failing
  expect_false( list(stop(1),5,2) %*>% max %>% .single_OK )
})
test_that('%*>% works the same of monad bound lists', {
  expect_equal(  funnel(3,5,2)       %*>% max %>% .single_value, 5                )
  expect_silent( funnel(stop(1),5,2) %*>% max                               )
  expect_equal(  funnel(stop(1),5,2) %*>% max %>% .single_value, list(NULL, 5, 2) )
  expect_false(  funnel(stop(1),5,2) %*>% max %>% .single_OK                      )
})
test_that('%*>% preserves keyword arguments', {
  expect_equal(5 %>>% funnel(y=1,z=2) %*>% { . + y + z } %>% esc, 8)
  expect_true(5 %>>% funnel(y=1,z=2) %*>% { . + y + z } %>% .single_OK)
  expect_equal(funnel(y=1,z=2) %*>% { y + z } %>% esc, 3)
  expect_true(funnel(y=1,z=2) %*>% { y + z } %>% .single_OK)
})
test_that('%*>% evaluates lists in variables', {
  # Tests for mishandling of NSE
  expect_equal(
    {
      args <- list(pattern="df", replacement="aa", x="asdf")
      args %*>% sub %>% esc
    },
    "asaa"
  )
  expect_equal( 1:6 %>>% { list(a=min(.), b=max(.)) } %*>% sum %>% esc, 7 )
})

test_that('anonymous expressions can be run', {
  expect_equal( 1:10 %>>% { 4 } %>% esc  , 4 )
  expect_true(  1:10 %>>% { 4 } %>% .single_OK     )
})

test_that('dot substitution is performed in anonymous expressions', {
  expect_equal( 1:10 %>>% { . * 2 } %>% esc  , (1:10)*2 )
  expect_true(  1:10 %>>% { . * 2 } %>% .single_OK            )

  expect_true(  cars %>>% { sapply(., is.numeric) %>% all } %>% esc  )
  expect_true(  cars %>>% { sapply(., is.numeric) %>% all } %>% .single_OK )
  expect_false( iris %>>% { sapply(., is.numeric) %>% all } %>% esc  )
  expect_true(  iris %>>% { sapply(., is.numeric) %>% all } %>% .single_OK )
})

test_that('"%__%" works', {
  expect_equal( stop("hi") %__% 1:10 %>% esc, 1:10 )
  expect_true(  stop("hi") %__% 1:10 %>% .single_OK )

  expect_equal( 1:5 %__% 1:10 %>% get_value(warn=FALSE), list(1:5, 1:10) )
  expect_true(  1:5 %__% 1:10 %>% .single_OK )
})
