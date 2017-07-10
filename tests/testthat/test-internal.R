context("internal methods")

# A better name than valid would be not empty
test_that(".is_valid_* do what I want them to", {

  expect_false ( .is_valid_integer ( "i"           ) )
  expect_false ( .is_valid_integer ( NA            ) )
  expect_false ( .is_valid_integer ( NA_integer_   ) )
  expect_false ( .is_valid_integer ( NULL          ) )
  expect_false ( .is_valid_integer ( integer(0)    ) )
  expect_true  ( .is_valid_integer ( 42L           ) )
  expect_true  ( .is_valid_integer ( 0L            ) )
  expect_true  ( .is_valid_integer ( -42L          ) )
  expect_true  ( .is_valid_integer ( 1:5           ) )

  expect_false ( .is_valid_real    ( "i"           ) )
  expect_false ( .is_valid_real    ( NA            ) )
  expect_false ( .is_valid_real    ( NA_real_      ) )
  expect_false ( .is_valid_real    ( NULL          ) )
  expect_false ( .is_valid_real    ( numeric(0)    ) )
  expect_true  ( .is_valid_real    ( 42.5          ) )
  expect_true  ( .is_valid_real    ( 0             ) )
  expect_true  ( .is_valid_real    ( -42           ) )
  expect_true  ( .is_valid_real    ( 1:5           ) )

  expect_false ( .is_valid_string  ( 5             ) )
  expect_false ( .is_valid_string  ( NA            ) )
  expect_false ( .is_valid_string  ( NA_character_ ) )
  expect_false ( .is_valid_string  ( NULL          ) )
  expect_false ( .is_valid_string  ( character(0)  ) )
  expect_true  ( .is_valid_string  ( "albatross"   ) )
  expect_true  ( .is_valid_string  ( letters       ) )

})
