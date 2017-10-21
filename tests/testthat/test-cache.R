context("cache methods")

test_that("voidCache works", {
  expect_silent(voidCache()@del())
  expect_equal(voidCache()@chk(), FALSE)
  expect_warning(voidCache()@get())
})

test_that("noCache works", {
  expect_silent(noCache()@del())
  expect_equal(noCache()@chk(), FALSE)
  expect_warning(noCache()@get())
})

test_that("memoryCache works", {
  # expect_equal({a <- memoryCache(5); a@del(); a@chk()}, FALSE)
  expect_equal({a <- memoryCache(5); a@get()}, 5)
  expect_equal({a <- memoryCache(5); a@chk()}, TRUE)
})

test_that("localCache works", {
  expect_equal(
    {
      f <- makeLocalCacher(".")
      a <- f(5)
      a@del()
      a@chk()
    },
    FALSE
  )
  expect_equal(
    {
      f <- makeLocalCacher(".")
      a <- f(5)
      x <- a@get()
      a@del()
      x
    },
    5
  )
  expect_equal(
    {
      f <- makeLocalCacher(".")
      a <- f(5)
      x <- a@chk()
      a@del()
      x
    },
    TRUE
  )
})
