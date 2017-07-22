context("code strings")


test_that("correct for %>>%", {
  expect_equal(
    5 %>>% sqrt %>>% sum %>% lapply(m_code),
    list("5", "sqrt", "sum")
  )
})

test_that("correct for funnel", {
  expect_equal(
    funnel(1) %>% as.list %>% lapply(m_code),
    list("1", "funnel(1)")
  )
  expect_equal(
    funnel(c(1,2)) %>% as.list %>% lapply(m_code),
    list("c(1, 2)", "funnel(c(1, 2))")
  )
  expect_equal(
    funnel(stop("hi")) %>% as.list %>% lapply(m_code),
    list('stop("hi")', 'funnel(stop("hi"))')
  )
  expect_equal(
    funnel(stop("hi"), sqrt(1)) %>% as.list %>% lapply(m_code),
    list("sqrt(1)", 'stop("hi")', 'funnel(stop("hi"), sqrt(1))')
  )
})

test_that("funnel taking from pipe (NOTE: may not be ideal)", {
  expect_equal(
    1:10 %>>% funnel(stop("hi"), sqrt(1)) %>% as.list %>% lapply(m_code),
    list("1:10", 'funnel(stop("hi"), sqrt(1))')
  )
  expect_equal(
    1:10 %>% funnel(stop("hi"), sqrt(1)) %>% as.list %>% lapply(m_code),
    list("sqrt(1)", 'stop("hi")', ".", 'funnel(., stop("hi"), sqrt(1))')
  )
})


test_that("Blocks are expanded into functions", {
  expect_equal(
    2 %>>% { . * 3 } %>%
      m_code %>%
      paste0(collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    "function(.){.*3}"
  )
})

# FIXME: this still isn't quite right
test_that("partially applied functions are as expected", {
  expect_equal(
    2 %*>% rbinom(size=3, prob=.2) %>%
      m_code %>%
      paste0(collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    "rbinom(size=3,prob=0.2)"
  )
})

test_that("%*>% doesn't do anything weird", {
  expect_equal(
    list(d=iris, i=2) %*>% { head(d, i) } %>%
      lapply(m_code) %>%
      lapply(paste0, collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    c('2', 'iris', 'list(d=iris,i=2)', 'function(d,i){head(d,i)}')
  )
})

test_that("funnel doesn't do anything weird", {
  expect_equal(
    funnel(d=iris, i=2) %*>% { head(d, i) } %>%
      lapply(m_code) %>%
      lapply(paste0, collapse=" ") %>%
      gsub(pattern=" ", replacement=""),
    c('2', 'iris', 'funnel(d=iris,i=2)', 'function(d,i){head(d,i)}')
  )
})
