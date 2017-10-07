context("rewriter functions")

test_that("format warnings", {
  expect_equal(
    -1 %>>%
      { list(format_warnings=function(x, w){ "oops" }); log(.) } %>%
      m_warnings,
    c("oops")
  )
  expect_equal(
    as_monad({
      list(format_warnings=function(x, w) { "oops" } )
      log(-1)
    }) %>% m_warnings,
    c("oops")
  )
  # Ensure the function isn't called when no warnings are raised
  expect_equal(
    as_monad({
      list(format_warnings=function(x, w) { "oops" } )
      sqrt(16)
    }) %>% esc, 4
  )
})
