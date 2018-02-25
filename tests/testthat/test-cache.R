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

key1="key1"
key2="key2"
test_that("local_cache works", {
  expect_equal(
    {
      f <- make_cacher()
      f@put(5, key1)
      # make ValueManager
      vm <- f@bld(key1)
      vm@del()
      vm@chk()
    },
    FALSE
  )
  expect_equal(
    {
      f <- make_cacher()
      f@put(5, key1)
      vm <- f@bld(key1)
      x <- vm@get()
      vm@del()
      x
    },
    5
  )
  expect_equal(
    {
      f <- make_cacher()
      f@put(5, key1)
      vm <- f@bld(key1)
      x <- vm@chk()
      vm@del()
      x
    },
    TRUE
  )
})
