context("monad dissection")

test_that("Tabulations at least don't fail catastrophically", {
  # one row for each warning, error, and note
  expect_equal(34 %>>% sqrt %>% missues %>% nrow, 0)

  # one row for each monad
  expect_equal(34 %>>% sqrt %>% mtabulate %>% nrow, 2)

  # ids should be sequential
  expect_equal({d <- 34 %>>% sqrt %>% mtabulate; d$id[2] - d$id[1]}, 1L)
})
