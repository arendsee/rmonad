context("magic")

foo <- function(x,k=1) { z = x + k; z + 1 }
sfoo <- substitute(function(x,k=1) { z = x + k; z + 1 })

test_that("extract the body of a function", {
  expect_equal(get_body(foo), body(foo))
  expect_equal(
    get_body(foo),
    get_body(sfoo)
  )
  expect_equal(get_body(substitute({k=1})), quote({k=1}))
  expect_error(get_body(4))
})

test_that("get_args function does", {
  expect_equal(get_args(foo), formals(foo))
  expect_equal(get_args(foo), get_args(sfoo))
})

bar <- function(x,k=1){
  j = x + 1
  k = 4
  j %>>% {
    i = j - 1
    rnorm(i, k)
  }
}
barbod <- body(bar)

test_that("get_preamble (expressions preceding the first monadic operator)", {
  expect_equal(get_preamble(bar), body(bar)[1:3])
})

test_that("get_declarations", {
  expect_equal(
    get_declarations(bar),
    list(barbod[[2]], barbod[[3]], barbod[[4]][[3]][[2]])
  )
})

test_that("Map bound variables in a function to a list", {
  expect_equal( get_bound_variables(sfoo, list(     )), list(       ) )
  expect_equal( get_bound_variables(sfoo, list(  1  )), list(x=1    ) )
  expect_equal( get_bound_variables(sfoo, list(  1,2)), list(x=1,k=2) )
  expect_equal( get_bound_variables(sfoo, list(k=2,1)), list(k=2,x=1) )
  expect_error( get_bound_variables(sfoo, list(u=2,1))                )
})


baz <- function(x,y,z){
  zanzibar <- x + y
  mozambique <- zanzibar + 1
  z <- 42
  istambul <- function(x) { x + 1 }
  5                    %>>%
    istambul           %>>%
    { . + z          } %>>%
    { . * zanzibar   } %>>%
    { . - mozambique }
}

test_that("Test little utilities", {
  expect_equal( get_rhs(substitute(2+3)), 3 )
  expect_equal( get_lhs(substitute(2+3)), 2 )
  expect_equal( get_rhs(substitute(x<-1+y)), quote(1+y) )
})


test_that("get_free_variables does", {
  expect_equal(get_free_variables(1), character(0))
  expect_equal(get_free_variables(substitute(x <- 1)), character(0))
  expect_equal(get_free_variables(substitute(x)), "x")
  expect_equal(get_free_variables(substitute({x;y})), c("x","y"))
  expect_equal(get_free_variables(substitute({x<-1;y})), c("y"))
  expect_equal(get_free_variables(substitute({x<-1;x})), character(0))
  expect_equal(get_free_variables(substitute({x;x<-1;y})), c("x","y"))
  expect_equal(get_free_variables(substitute(x+y+x)), c("x","y"))
  expect_equal(
    get_free_variables(
      function(x) y
    ), "y")
  expect_equal(
    get_free_variables(
      function(x) { 2 %>% { . + . } }
    ), character(0))
  expect_equal(
    get_free_variables(
      function(x) { . %>% { . + . } }
    ), '.')
  expect_equal(
    get_free_variables(
      function(x) { function(y) y }
    ), character(0))
  expect_equal(
    get_free_variables(
      function(x) { function(y) z }
    ), "z")
  expect_equal(
    get_free_variables(
      function(x) { y = 1; y }
    ), character(0))
})

test_that("Multiple free variables can be handled in declarations", {
  expect_equal(
    3 %>>%
    {
      y <- 1 
      bar <- . + y
      bar %>>% sqrt # this test MUST have a monadic operator
    } %>% esc,
    2
  )
})
