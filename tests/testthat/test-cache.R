context("cache methods")

test_that("void_cache works", {
  expect_silent(void_cache()@del())
  expect_equal(void_cache()@chk(), FALSE)
  expect_warning(void_cache()@get())
})

test_that("no_cache works", {
  expect_silent(no_cache()@del())
  expect_equal(no_cache()@chk(), FALSE)
  expect_warning(no_cache()@get())
})

test_that("memory_cache works", {
  # expect_equal({a <- memory_cache(5); a@del(); a@chk()}, FALSE)
  expect_equal({a <- memory_cache(5); a@get()}, 5)
  expect_equal({a <- memory_cache(5); a@chk()}, TRUE)
})

test_that("local_cache works", {
  expect_equal(
    {
      f <- make_local_cacher(".")
      a <- f(5)
      a@del()
      a@chk()
    },
    FALSE
  )
  expect_equal(
    {
      f <- make_local_cacher(".")
      a <- f(5)
      x <- a@get()
      a@del()
      x
    },
    5
  )
  expect_equal(
    {
      f <- make_local_cacher(".")
      a <- f(5)
      x <- a@chk()
      a@del()
      x
    },
    TRUE
  )
})
