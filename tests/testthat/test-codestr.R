context("code strings")


test_that("correct for %>>%", {
  expect_equal(
    5 %>>% sqrt %>>% sum %>% lapply(m_code),
    list("5", "sqrt", "sum")
  )
})
