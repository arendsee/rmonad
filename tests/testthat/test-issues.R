context("github issues")

test_that('issue #1: "%__%" does not double nest lhs', {
  expect_silent(5 %__% as_monad(4) %>>% sqrt)
  expect_equal(5 %__% as_monad(4) %>>% sqrt %>% .single_value, 2)
})

test_that('issue #2: funnel works with %__%', {
  expect_equal(
    funnel(16 %v>% sqrt) %>% get_value(warn=FALSE),
    list(16, NULL, list(4))
  )
  expect_equal(
    "hi" %__%
      funnel(16 %v>% sqrt) %>% get_value(warn=FALSE),
    list("hi", 16, NULL, list(4))
  ) 
})

test_that('issue #3: nested errors are localized', {
  expect_equal(
    "a" %>>% {
      "Level One"
      . %>>% paste("b") %>>% {
        "Level Two"
        . %>>% paste("c") %>>% stop
      }
    } %>% get_value(warn=FALSE),
    list(NULL, NULL, NULL, NULL, NULL, "a b c", "a b", "a")
  )
  # The %__% operator often screws things up, best to check
  expect_equal(
    "yolo" %__%
    "a" %>>% {
      "Level One"
      . %>>% paste("b") %>>% {
        "Level Two"
        . %>>% paste("c") %>>% stop
      }
    } %>% get_value(warn=FALSE),
    list("yolo", NULL, NULL, NULL, NULL, NULL, "a b c", "a b", "a")
  )
})
