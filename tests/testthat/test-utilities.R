context("utility functions")

a11 = "foo"
a1n = c("foo", "bar", "baz")
ann = list(c("foo", "bar"), "foo", c("boo", "bad"), c("foo", "bar", "baz"))

test_that(".a_has_prefix_b - against self", {
  expect_true(.a_has_prefix_b(a11, a11))
  expect_true(.a_has_prefix_b(a1n, a1n))
  expect_true(all(.a_has_prefix_b(ann, ann)))
})

test_that(".a_has_prefix_b - against other", {
  expect_false(.a_has_prefix_b(a11, "bar"))
  # match to wrong location fails
  expect_false(.a_has_prefix_b(a1n, "bar"))
  # match failing in latter elements fails 
  expect_false(.a_has_prefix_b(a1n, c("foo", "baz")))
  expect_false(any(.a_has_prefix_b(ann, "boondock")))
})

test_that(".a_has_prefix_b - multiple match", {
  expect_equal(.a_has_prefix_b(ann, c("foo","bar")), c(T,F,F,T))
  expect_equal(.a_has_prefix_b(ann, list(c("foo","bar"), "boo")), c(T,F,T,T))
  expect_false(.a_has_prefix_b(a11, list(c("foo","bar"), "boo")))
  expect_true(.a_has_prefix_b(a11, list(c("foo","bar"), "boo", "foo")))
})
