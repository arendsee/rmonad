context("time and space")

test_that("all operations are timed", {
  expect_true(runif(1e6) %>>% sqrt %>>% sum %>%
              sapply(m_time) %>% { all(. >= 0) })
})

test_that("space is recorded for all steps", {
  expect_true(runif(1e6) %>>% sqrt %>>% sum %>%
              sapply(m_mem) %>% { all(. >= 0) })
})
