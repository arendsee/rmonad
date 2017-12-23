context("monad laws")

# rmonad twists the monad pattern a bit by splitting the factor `(a -> m b)`
# from the bind function:

#   bind :: m a -> (a -> m b) -> m b

# into `(a -> b)`, which is provided by the user, and the internal evaluation
# function `(a -> b) -> a -> m b`, which evaluates the functional application,
# wrapping the result in an Rmonad object. This second function is `as_monad`.

test_that("First law", {
  # left identity
  # return a >>= f  ===  f a
  # ------------------------

  # rmonad is a bit sloppy in this case. If the LFS is not an Rmonad, it first
  # evaluates it into an Rmonad with `as_monad`.
  # So the expression, `16 %>>% sqrt` is identical to `as_monad(16) %>>% sqrt`.
  expect_true(rmonad_equal(16 %>>% sqrt, as_monad(16) %>>% sqrt))

  # The way bind works in rmonad is a bit odd, the Haskell bind signature is:
  #   bind :: m a -> (a -> m b) -> m b
  # But the rmonad signature is, superficially, closer to:
  #   bindR :: m a -> (a -> b) -> m b

  # However, internally `as_monad` takes the lhs `a` and the rhs `(a -> b)` and
  # applies them to get `m b`. Effectively: `(a -> (a -> b)) -> m b`. Thus,
  # overall, bindR has the proper signature.

  # By the first law, `a %>>% f` should be the same as `f(a)`, however we have
  # to account for the two relaxations above.

  # NOTE: the acrobatics below really make teasing out the monad tricky. I fear
  # that I don't really have one anymore.

})

# test_that("Second law", {
#   # right identity
#   # m >>= return  ===  m
#
#   # NOTE: Unfortunately, this currently isn't quite true
#   # expect_true(rmonad_equal(16 %>>% sqrt, 16 %>>% sqrt %>>% as_monad))
# })
#
# test_that("Third law", {
#
#   addone <- function(x) x+1
#   addtwo <- function(x) x+2
#
#   # associative law
#   # (m >>= f) >>= g  ===  m >>= (\x -> f x >>= g)
#
#   expect_true(rmonad_equal(
#     (1 %v>% addone) %v>% addtwo,
#     1 %v>% (addone(.) %v>% addtwo)
#   ))
# })
