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
    funnel(
      {warning("goo"); warning("roo")} %__% warning("poo") %>% esc(quiet=TRUE)
    ) %>% ms_warnings,
    list(c("goo", "roo", "poo"), character(0))
  )
  expect_equal(
    funnel(warning("goo") %__% warning("roo") %>% esc(quiet=TRUE)) %>% ms_warnings,
    list(c("goo", "roo"), character(0))
  )
  expect_equal(
    funnel(-2:3 %>>% sqrt %>>% sum %>% esc(quiet=TRUE)) %>% ms_warnings,
    list("NaNs produced", character(0))
  )
})
