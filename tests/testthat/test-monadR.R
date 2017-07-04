context("rmonad.R")

m1 <- new(
  "Rmonad",
  x = list(42),
  stage = new(
    "record",
    code     = "",
    warnings = list("a", "s"),
    notes    = list("q", "w")
  )
)

e2 <- new(
  "Rmonad",
  OK     = FALSE,
  stage  = new("record", errors = list("Oh no Mr. Wizard!"))
)

m1e <- new(
  "Rmonad",
  OK     = FALSE,
  history = list(
    new(
      "record",
      warnings = list("a", "s"),
      notes    = list("q", "w")
    ),
    new(
      "record",
      errors = list("Oh no Mr. Wizard!")
    )
  )
)

m2 <- new(
  "Rmonad",
  x = list(82),
  stage = new(
    "record",
    warnings = list("d", "f"),
    notes    = list("e", "r")
  )
)

m12 <- new(
  "Rmonad",
  x = list(42,82),
  history = list(
    new(
      "record",
      warnings = list("a", "s"),
      notes    = list("q", "w")
    ),
    new(
      "record",
      warnings = list("d", "f"),
      notes    = list("e", "r")
    )
  )
)

m1yolo <- new(
  "Rmonad",
  x = list(42,"yolo"),
  history = list(
    new(
      "record",
      warnings = list("a", "s"),
      notes    = list("q", "w")
    ),
    new(
      "record",
      code="x"
    )
  )
)

test_that("combine works", {
  expect_equal(combine(list(m1, m2)), m12)
  expect_equal(combine(list(m1, e2)), m1e)
  expect_equal(combine(list(m1, "yolo")), m1yolo)
})

test_that("Monad casting works", {
  expect_equal(as_rmonad(12), new("Rmonad", x=list(12), stage=new("record", code="x")))
  expect_equal(as_rmonad(m1), m1)
  expect_equal(as_rmonad(e2), e2)
})

test_that(
  "pass, fail, warn, and note work on monadic input",
  {
    expect_equal(pass(m1), m1)
    expect_equal(note(m1, "sit!"), { m <- m1; m@stage@notes    <- append(m@stage@notes,    "sit!"); m })
    expect_equal(warn(m1, "run!"), { m <- m1; m@stage@warnings <- append(m@stage@warnings, "run!"); m })
    expect_equal(fail(m1, "die!"), { m <- m1; m@stage@errors   <- append(m@stage@errors,   "die!"); m@OK=FALSE; m })
  }
)

test_that(
  "pass, fail, warn, and note work on non-monadic input",
  {
    expect_equal(
      pass(42),
      new("Rmonad",x=list(42), stage=new("record", code="42"))
    )

    expect_equal(
      note(42, "sit!"),
      new("Rmonad", x=list(42), stage=new("record", code="42", notes=list("sit!")))
    )

    expect_equal(
      warn(42, "run!"),
      new(
        "Rmonad",
        x     = list(42),
        stage = new("record", code="42", warnings = list("run!"))
      )
    )

    expect_equal(
      fail(42, "die!"),
      new(
        "Rmonad",
        OK=FALSE,
        stage = new(
          "record",
          code="42",
          errors = list("die!")
        )
      )
    )
  }
)

m1b <- new(
  "Rmonad",
  OK     = FALSE,
  stage  = new(
             "record",
             errors = list("die!")
           ),
  history = list(
    new(
      "record",
      warnings = list("a", "s"),
      notes    = list("q", "w")
    )
  )
)

m12b <- new(
  "Rmonad",
  x = list(84),
  history = list(
    new(
      "record",
      warnings = list("a", "s"),
      notes    = list("q", "w")
    )
  )
)

mfp <- function(x) { pass(2 * x) }
mfe <- function(x) { fail(x, "die!") }
nf  <- function(x) { 2 * x }

test_that(
  "bind always propagates failed states",
  {
    expect_equal(bind(m1b, mfp), m1b)
    expect_equal(bind(m1b, mfe), m1b)
    expect_equal(bind(m1b, nf ), m1b)
  }
)

test_that(
  "bind functions may be monadic or not",
  {
    expect_equal(bind(m1, mfp), {m <- m12b; m@stage@code = "mfp"; m })
    expect_equal(bind(m1, nf ), {m <- m12b; m@stage@code = "nf";  m })
  }
)

new_me <- new(
  "Rmonad",
  OK    = FALSE,
  stage = new(
    "record",
    errors=list("die!")
  ),
  history = list(new("record", code="42"))
)

new_mp <- new(
  "Rmonad",
  x     = list(84),
  stage = new("record"),
  history = list(new("record", code="42"))
)

test_that(
  "bind works with non-monadic input",
  {
    expect_equal(bind(42, mfp), {m <- new_mp; m@stage@code = "mfp"; m })
    expect_equal(bind(42, mfe), {m <- new_me; m@stage@code = "mfe"; m })
    expect_equal(bind(42,  nf), {m <- new_mp; m@stage@code = "nf";  m })
  }
)

test_that(
  "operators work",
  {
    expect_equal( m1e %>>% mfp , m1e )
    expect_equal( m1e %>>% mfe , m1e )
    expect_equal( m1e %>>% nf  , m1e )
    expect_equal( m1  %>>% mfp , {m <- m12b; m@stage@code = "mfp";  m } )
    expect_equal( m1  %>>% nf  , {m <- m12b; m@stage@code = "nf";   m } )
    expect_equal( 42  %>>% mfp , { m <- new_mp; m@stage@code = "mfp"; m } )
    expect_equal( 42  %>>% mfe , { m <- new_me; m@x <- list(); m@stage@code = "mfe"; m } )
    expect_equal( 42  %>>% nf  , { m <- new_mp; m@stage@code = "nf"; m } )
  }
)
