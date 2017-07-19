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
  expect_error( get_bound_variables(sfoo, list(u=2,1)) )
})
