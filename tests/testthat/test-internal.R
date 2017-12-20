context("internal methods")

# A better name than valid would be not empty
test_that(".is_not_empty_* do what I want them to", {

  expect_false ( .is_not_empty_real    ( "i"           ) )
  expect_false ( .is_not_empty_real    ( NA            ) )
  expect_false ( .is_not_empty_real    ( NA_real_      ) )
  expect_false ( .is_not_empty_real    ( NULL          ) )
  expect_false ( .is_not_empty_real    ( numeric(0)    ) )
  expect_true  ( .is_not_empty_real    ( 42.5          ) )
  expect_true  ( .is_not_empty_real    ( 0             ) )
  expect_true  ( .is_not_empty_real    ( -42           ) )
  expect_true  ( .is_not_empty_real    ( 1:5           ) )

  expect_false ( .is_not_empty_string  ( 5             ) )
  expect_false ( .is_not_empty_string  ( NA            ) )
  expect_false ( .is_not_empty_string  ( NA_character_ ) )
  expect_false ( .is_not_empty_string  ( NULL          ) )
  expect_false ( .is_not_empty_string  ( character(0)  ) )
  expect_true  ( .is_not_empty_string  ( "albatross"   ) )
  expect_true  ( .is_not_empty_string  ( letters       ) )

})
