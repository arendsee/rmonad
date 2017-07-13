context("non-standard evaluation")

strlambda <- substitute({
  "this is not a docstring"
})
strlambda_out <- list(expr=strlambda, docstring=NULL)

expr <- substitute({
  2 * 2
  5 * x
})
expr_out <- list(expr=expr, docstring=NULL)

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
expr_doc_mag_out <- list(expr=expr_doc_mag, docstring=NULL)

test_that("when expression is not a lambda", {
  expect_equal(extract_docstring(2), list(expr=2, docstring=NULL))
  expect_equal(extract_docstring("adsf"), list(expr="adsf", docstring=NULL))
  expect_equal(extract_docstring(c("adsf", "df")), list(expr=c("adsf", "df"), docstring=NULL))
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

test_that("docstrings work on rhs", {
  expect_equal(4 %>>% {"hi"; sqrt(.)} %>% esc, 2)
  expect_true( 4 %>>% {"hi"; sqrt(.)} %>% m_OK)
  expect_equal(4 %>>% {"hi"; sqrt(.)} %>% m_doc, "hi")
})

test_that("docstrings work on lhs", {
  expect_equal({"adsf"; 4} %>>% sqrt %>% esc, 2)
  expect_true( {"adsf"; 4} %>>% sqrt %>% m_OK)
  expect_equal({"adsf"; 4} %>>% sqrt %>% lapply(m_doc), list("adsf", NULL))
})

test_that("docstrings work with %__%", {
  expect_equal(
    {"qwer"; 5} %__%
    {"asdf"; 4} %>>%
    sqrt        %>% lapply(m_doc),
    list("qwer", "asdf", NULL)
  )
})

test_that("as_monad handles docstrings", {
  expect_equal(as_monad({"asdf"; 5}) %>% m_doc, "asdf")
  expect_equal(as_monad({"asdf"; 5}) %>% esc, 5)
  # no funny business is going on ...
  expect_equal(as_monad({"asdf"; 5}) %>>% '*'(6) %>% esc, 30)
})
