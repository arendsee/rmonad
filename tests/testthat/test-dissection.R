context("monad dissection")

test_that("Tabulations at least don't fail catastrophically", {
  # one row for each warning, error, and note
  expect_equal(34 %>>% sqrt %>% missues %>% nrow, 0)

  # one row for each monad
  expect_equal(34 %>>% sqrt %>% mtabulate %>% nrow, 2)
})

test_that("missues finds the correct errors and warnings", {
  expect_silent(234 %>>% sqrt %>% esc)
  expect_warning(warning("foo") %__% 5 %>% esc, "foo") 
  expect_equal(
    lapply(
      funnel({warning("goo"); warning("roo")} %__% warning("poo") %>% esc(quiet=TRUE)),
      m_warnings
    )[[1]],
    c("goo", "roo", "poo")
  )
  expect_equal(
    lapply(
      funnel(warning("goo") %__% warning("roo") %>% esc(quiet=TRUE)),
      m_warnings
    )[[1]],
    c("goo", "roo")
  )
  expect_equal(
    lapply(
      funnel(-2:3 %>>% sqrt %>>% sum %>% esc(quiet=TRUE)),
      m_warnings
    )[[1]],
    "NaNs produced"
  )
})
