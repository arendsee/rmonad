context("code strings")


test_that("correct for %>>%", {
  expect_equal(
    5 %>>% sqrt %>>% sum %>% lapply(m_code),
    list("5", "sqrt", "sum")
  )
})

test_that("correct for funnel", {
  expect_equal(
    funnel(1) %>% as.list %>% lapply(m_code),
    list("1", "funnel(1)")
  )
  expect_equal(
    funnel(c(1,2)) %>% as.list %>% lapply(m_code),
    list("c(1, 2)", "funnel(c(1, 2))")
  )
  expect_equal(
    funnel(stop("hi")) %>% as.list %>% lapply(m_code),
    list('stop("hi")', 'funnel(stop("hi"))')
  )
  expect_equal(
    funnel(stop("hi"), sqrt(1)) %>% as.list %>% lapply(m_code),
    list("sqrt(1)", 'stop("hi")', 'funnel(stop("hi"), sqrt(1))')
  )
})

test_that("funnel taking from pipe (NOTE: may not be ideal)", {
  expect_equal(
    1:10 %>>% funnel(stop("hi"), sqrt(1)) %>% as.list %>% lapply(m_code),
    list("1:10", 'funnel(stop("hi"), sqrt(1))')
  )
  expect_equal(
    1:10 %>% funnel(stop("hi"), sqrt(1)) %>% as.list %>% lapply(m_code),
    list("sqrt(1)", 'stop("hi")', ".", 'funnel(., stop("hi"), sqrt(1))')
  )
})
