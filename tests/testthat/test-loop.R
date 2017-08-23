context("looping in pipelines")

foo <- function(x){
  sqrt(x) %v>% is.na
}

test_that("Lists over nested functions produce the correct output", {
  # the results is a list of Rmonads
  expect_equal(
    -1:1 %>>% { lapply(., foo) } %>% {sapply(m_value(.), is_rmonad)},
    c(TRUE, TRUE, TRUE)
  )
  # binding this to `combine` performs the operation
  #   m [m a] -> m [a]
  expect_equal(
    -1:1 %>>% { lapply(., foo) } %>>% combine %>% m_value,
    list(TRUE, FALSE, FALSE)
  )

  expect_equal(
    -1:1 %>%  { lapply(., foo) } %>%  combine %>% m_value,
    -1:1 %>>% { lapply(., foo) } %>>% combine %>% m_value
  )
})
