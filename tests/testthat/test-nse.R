context("non-standard evaluation")

s <- substitute

test_that("when expression is not a lambda", {
  expect_equal(
    extract_metadata(2)[1:3],
    list(expr=2, docstring=.default_doc(), metadata=s(list()))
  )
  expect_equal(
    extract_metadata("adsf")[1:3],
    list(expr="adsf", docstring=.default_doc(), metadata=s(list()))
  )
  expect_equal(
    extract_metadata(c("adsf", "df"))[1:3],
    list(expr=c("adsf", "df"), docstring=.default_doc(), metadata=s(list()))
  )
})


foo <- function(x, y) { "foofoo"; list(k=1); x + y }
e=environment()
test_that("when expression is a named function", {
  expect_equal(
    extract_metadata(substitute(foo), env=e, skip_name=FALSE)[1:3],
    list(expr=substitute(foo), docstring="foofoo", metadata=s(list(k=1)))
  )
  expect_equal(
    extract_metadata(substitute(foo(y=2)), env=e)[1:3],
    list(expr=substitute(foo(y=2)), docstring="foofoo", metadata=s(list(k=1)))
  )
})

test_that("when lambda is only a string", {
  expect_equal(
    extract_metadata(substitute({"this is not a docstring"}))[1:3],
    list(
      expr      = substitute({"this is not a docstring"}),
      docstring = .default_doc(),
      metadata  = s(list())
    )
  )
})

test_that("when lambda has no docstring", {
  expect_equal(
    extract_metadata(substitute({2*2;5*x}))[1:3],
    list(
      expr=substitute({2*2;5*x}),
      docstring=.default_doc(),
      metadata=s(list())
    )
  )
})

test_that("when lambda has docstring", {
  expect_equal(
    extract_metadata(substitute({"this is a docstring";NULL}))[1:3],
    list(
      expr=substitute({NULL}),
      docstring="this is a docstring",
      metadata=s(list())
    )
  )
})

test_that("when lambda starts with string that is part of an expression", {
  expect_equal(
    extract_metadata(substitute({"this is not a docstring" %>% foo;NULL}))[1:3],
    list(
      expr=substitute({"this is not a docstring" %>% foo;NULL}),
      docstring=.default_doc(),
      metadata=s(list())
    )
  )
})

test_that("docstrings work on rhs", {
  expect_equal(4 %>>% {"hi"; sqrt(.)} %>% esc, 2)
  expect_true( 4 %>>% {"hi"; sqrt(.)} %>% .single_OK)
  expect_equal(4 %>>% {"hi"; sqrt(.)} %>% .single_doc, "hi")
})

test_that("docstrings work on lhs", {
  expect_equal({"adsf"; 4} %>>% sqrt %>% esc, 2)
  expect_true( {"adsf"; 4} %>>% sqrt %>% .single_OK)
  expect_equal({"adsf"; 4} %>>% sqrt %>% get_doc, list("adsf", .default_doc()))
})

test_that("docstrings work with %__%", {
  expect_equal(
    {"qwer"; 5} %__%
    {"asdf"; 4} %>>%
    sqrt        %>% get_doc,
    list("qwer", "asdf", .default_doc())
  )
})

test_that("evalwrap handles docstrings", {
  expect_equal(evalwrap({"asdf"; 5}) %>% .single_doc, "asdf")
  expect_equal(evalwrap({"asdf"; 5}) %>% esc, 5)
  # no funny business is going on ...
  expect_equal(evalwrap({"asdf"; 5}) %>>% prod(6) %>% esc, 30)
  expect_equal(evalwrap({}) %>% esc, NULL)

  expect_true(
    {
      x <- evalwrap({"asdf"; list(k=1); 5})
      .single_doc(x) == "asdf" && identical(.single_meta(x), list(k=1))
    }
  )

})

test_that("anonymous functions handle docstrings and metadata", {
  expect_equal(16 %>>% (function(x){"asdf"; sqrt(x)}) %>% .single_doc, "asdf")
  expect_equal(16 %>>% (function(x){"asdf"; sqrt(x)}) %>% esc, 4)

  expect_equal(16 %>>% (function(x){list(k=1); sqrt(x)}) %>% .single_meta, list(k=1))
  expect_equal(16 %>>% (function(x){list(k=1); sqrt(x)}) %>% esc, 4)

  expect_true(
    {
      x <- 16 %>>% (function(x){"asdf"; list(k=1); sqrt(x)})
      .single_doc(x) == "asdf" && identical(.single_meta(x), list(k=1))
    }
  )
})

test_that("metadata is extracted", {
  expect_equal(
    extract_metadata(substitute({list(k=1); NULL}))[1:3],
    list(
      expr      = substitute({NULL}),
      docstring = .default_doc(),
      metadata  = s(list(k=1))
    )
  )
  expect_equal(
    extract_metadata(substitute({"asdf"; list(k=1); x + y}))[1:3],
    list(
      expr      = substitute({x + y}),
      docstring = "asdf",
      metadata  = s(list(k=1))
    )
  )
})

test_that("docstrings are correct in anonymous bind expressions", {
  expect_equal(
    16 %>>% {"asdf"; list(k=1); sqrt(.)} %>%
       .single_code %>% gsub(pattern=" |\n", replacement="") %>%
       paste(collapse=""),
    'function(.){sqrt(.)}'
  )
})

test_that("metadata lists are evaluated in the proper environment", {
  expect_equal(
    {
      y <- 42
      36 %>>% {
        list(foo = y)
        NULL
      } %>% .single_meta %>% { .$foo }
    },
    42
  )
  expect_equal(
    {
      do_error <- function(x, ws){ "dang it" }
      "wrench" %>>% {
        list(format_error=do_error)
        log(.)
      } %>% .single_error
    },
    "dang it"
  )
})


foo <- function(x, k=1) { list(x=2*x); x }
bar <- (function(){ fluf = 9; function(y, k=1) { list(x=fluf); fluf }})()
bomb <- function(x){ list(stop("on no!")); x }
test_that("metadata are evaluated in function scope", {
  # Can access arguments
  expect_equal(
    5 %>>% foo %>% get_meta, list(list(), list(x=10))
  )
  # Can access enclosing scope
  expect_equal(
    5 %>>% bar %>% get_meta, list(list(), list(x=9))
  )
  # Errors in the metadata do not blow up the pipeline
  expect_equal(
    5 %>>% bomb %>% get_meta, list(list(), list(.metadata_error="on no!"))
  )
})


test_that("Metadata works for variable intput", {
  ####### TODO: reinstate these test ###########################################
  # label <- function(y){
  #   function(x,ws) paste0(y, ": ", ws)
  # }
  # bar <- function(x, y) {
  #   list(
  #     format_warnings=label(y)
  #   )
  #   warning("B")
  #   x
  # }
  # foo <- function(a, prefix){
  #   a %>>% bar(y=prefix)
  # }
  #
  # # In v0.5.0, this dies screaming that "object prefix is not found":
  # expect_warning(foo(5, prefix="A") %>% esc, "A: B")
  # expect_warning(5 %>>% bar(y=foofoo) %>% esc, "A: B")
  # foofoo = "A"
  # expect_warning(5 %>>% bar(y="A") %>% esc, "A: B")
  #############################################################################
})
