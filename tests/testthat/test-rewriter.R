context("rewriter functions")

test_that("format warnings", {
  expect_equal(
    -1 %>>%
      { list(format_warnings=function(x, w){ "oops" }); log(.) } %>%
      .single_warnings,
    c("oops")
  )
  expect_equal(
    as_monad({
      list(format_warnings=function(x, w) { "oops" } )
      log(-1)
    }) %>% .single_warnings,
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

test_that("format warnings with output data", {
  expect_equal(
    -1 %>>%
      { list(format_warnings=function(x, w){ paste0("made a ", x) }); log(.) } %>%
      .single_warnings,
    c("made a NaN")
  )
})
