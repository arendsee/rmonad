context("code strings")

test_that("evalwrap captures code string", {
  expect_equal(get_code(evalwrap(41+1))[[1]], '41 + 1') 
})

test_that("correct for %>>%", {
  expect_equal(
    5 %>>% sqrt %>>% sum %>% get_code,
    list("5", "sqrt", "sum")
  )
})

test_that("correct for funnel", {
  expect_equal(
    funnel(1) %>% get_code,
    list("1", "funnel(1)")
  )
  expect_equal(
    funnel(c(1,2)) %>% get_code,
    list("c(1, 2)", "funnel(c(1, 2))")
  )
  expect_equal(
    funnel(stop("hi")) %>% get_code,
    list('stop("hi")', 'funnel(stop("hi"))')
  )
  expect_equal(
    funnel(stop("hi"), sqrt(1)) %>% get_code,
    list("sqrt(1)", 'stop("hi")', 'funnel(stop("hi"), sqrt(1))')
  )
})

test_that("funnel taking from pipe", {
  # # TODO: better handling for this case
  # expect_equal(
  #   1:10 %>>% funnel(stop("hi"), sqrt(1)) %>% get_code,
  #   list("1:10", 'funnel(stop("hi"), sqrt(1))')
  # )
  expect_equal(
    1:10 %>% funnel(stop("hi"), sqrt(1)) %>% get_code,
    list("sqrt(1)", 'stop("hi")', ".", 'funnel(., stop("hi"), sqrt(1))')
  )
})


test_that("Blocks are expanded into functions", {
  expect_equal(
    2 %>>% { . * 3 } %>%
      .single_code %>%
      paste0(collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    "function(.){.*3}"
  )
})

# FIXME: this still isn't quite right
test_that("partially applied functions are as expected", {
  expect_equal(
    2 %*>% rbinom(size=3, prob=.2) %>%
      .single_code %>%
      paste0(collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    "rbinom(size=3,prob=0.2)"
  )
})

test_that("%*>% produces the correct code", {
  expect_equal(
    list(d=iris, i=2) %*>% { head(d, i) } %>%
      get_code %>%
      lapply(paste0, collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    c('2', 'iris',
      'list(d=iris,i=2)', # FIXME: can we prune this node?
      'function(d,i){head(d,i)}')
  )
})

test_that("funnel produces correct code", {
  expect_equal(
    funnel(d=iris, i=2) %*>% { head(d, i) } %>%
      get_code %>%
      lapply(paste0, collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    c('2', 'iris',
      'funnel(d=iris,i=2)', # FIXME: can we prune this node?
      'function(d,i){head(d,i)}')
  )
})
