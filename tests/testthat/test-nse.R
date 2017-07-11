context("non-standard evaluation")

strlambda <- substitute({
  "this is not a docstring"
})
strlambda_out <- list(expr=strlambda, docstring="")

expr <- substitute({
  2 * 2
  5 * x
})
expr_out <- list(expr=expr, docstring="")

expr_doc <- substitute({
  "this is a docstring"
  2 * 2
  5 * x
})
expr_doc_out <- list(expr=expr, docstring="this is a docstring")

expr_doc_mag <- substitute({
  "this is not a docstring" %>% toupper
  2 * 2
  5 * x
})
expr_doc_mag_out <- list(expr=expr_doc_mag, docstring="")

test_that("when expression is not a lambda", {
  expect_equal(extract_docstring(2), list(expr=2, docstring=""))
  expect_equal(extract_docstring("adsf"), list(expr="adsf", docstring=""))
  expect_equal(extract_docstring(c("adsf", "df")), list(expr=c("adsf", "df"), docstring=""))
})

test_that("when lambda is only a string", {
  expect_equal(extract_docstring(strlambda), strlambda_out)
})

test_that("when lambda has no docstring", {
  expect_equal(extract_docstring(expr), expr_out)
})

test_that("when lambda has docstring", {
  expect_equal(extract_docstring(expr_doc), expr_doc_out)
})

test_that("when lambda starts with string that is part of an expression", {
  expect_equal(extract_docstring(expr_doc_mag), expr_doc_mag_out)
})
