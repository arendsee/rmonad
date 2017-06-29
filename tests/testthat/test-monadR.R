context("monadR")

zero <- new(
  "monadR",
  x        = list(),
  warnings = list(),
  notes    = list(),
  errors   = list(),
  OK       = TRUE
)

m1 <- new(
  "monadR",
  x        = list(1),
  warnings = list("a", "s"),
  notes    = list("q", "w")
)
m2 <- new(
  "monadR",
  x        = list(2),
  warnings = list("d", "f"),
  notes    = list("e", "r")
)
e2 <- new(
  "monadR",
  OK     = FALSE,
  errors = list("Oh no Mr. Wizard!")
)
m12 <- new(
  "monadR",
  x        = list(1,2),
  warnings = list("a", "s", "d", "f"),
  notes    = list("q", "w", "e", "r")
)
m1e <- new(
  "monadR",
  warnings = list("a", "s"),
  notes    = list("q", "w"),
  errors   = list("Oh no Mr. Wizard!"),
  OK       = FALSE
)

test_that("mcombine works", {
  expect_equal(mcombine(m1, m2), m12)
  expect_equal(mcombine(m1, e2), m1e)
  expect_equal(mcombine(m1, "yolo"), { m1@x <- append(m1@x, "yolo"); m1 } )
})

test_that("mzero works", {
  expect_equal(new("monadR"), zero)
  expect_equal(mzero(), zero)
})

test_that("Monad casting works", {
  expect_equal(as.monadR(1), new("monadR", x=list(1)))
  expect_equal(as.monadR(m1), m1)
})

test_that(
  "pass, fail, warn, and note work on monadic input",
  {
    expect_equal(pass(m1), m1)
    expect_equal(note(m1, "sit!"), { m <- m1; m@notes    <- append(m@notes,    "sit!"); m })
    expect_equal(warn(m1, "run!"), { m <- m1; m@warnings <- append(m@warnings, "run!"); m })
    expect_equal(fail(m1, "die!"), { m <- m1; m@errors   <- append(m@errors,   "die!"); m@OK=FALSE; m })
  }
)

test_that(
  "pass, fail, warn, and note work on non-monadic input",
  {
    expect_equal(pass(42),         new("monadR", x=list(42)))
    expect_equal(note(42, "sit!"), new("monadR", x=list(42), notes    = list("sit!")))
    expect_equal(warn(42, "run!"), new("monadR", x=list(42), warnings = list("run!")))
    expect_equal(fail(42, "die!"), new("monadR",             errors   = list("die!"), OK=FALSE))
  }
)

m3 <- new(
  "monadR",
  x        = list(42),
  warnings = list("a", "s"),
  notes    = list("q", "w")
)
m3e <- new(
  "monadR",
  x        = list(),
  errors   = list("die m3e!"),
  warnings = list("a", "s"),
  notes    = list("q", "w"),
  OK       = FALSE
)
m4 <- new(
  "monadR",
  x        = list(84),
  warnings = list("a", "s"),
  notes    = list("q", "w")
)
mfp <- function(x) { pass(2 * x) }
mfe <- function(x) { fail(x, "die!") }
nf  <- function(x) { 2 * x }

new_me <- new("monadR", errors=list("die!"), OK=FALSE)
new_mp <- new("monadR", x=list(84))

test_that(
  "bind always propagates failed states",
  {
    expect_equal(bind(m3e, mfp), m3e)
    expect_equal(bind(m3e, mfe), m3e)
    expect_equal(bind(m3e, nf ), m3e)
  }
)

test_that(
  "bind functions may be monadic or not",
  {
    expect_equal(bind(m3, mfp), m4)
    expect_equal(bind(m3, nf ), m4)
  }
)

test_that(
  "bind works with non-monadic input",
  {
    expect_equal(bind(42, mfp), new_mp)
    expect_equal(bind(42, mfe), new_me)
    expect_equal(bind(42,  nf), new_mp)
  }
)
