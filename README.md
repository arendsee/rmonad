[![Travis-CI Build Status](https://travis-ci.org/arendsee/rmonad.svg?branch=master)](https://travis-ci.org/arendsee/rmonad)
[![Coverage Status](https://img.shields.io/codecov/c/github/arendsee/rmonad/master.svg)](https://codecov.io/github/arendsee/rmonad?branch=master)

# Rmonad

`rmonad` allows state to be preserved across chains of operations.

 * errors, warnings and messages are anchored to their expressions

 * when an error occurs, the valid data in the node preceding it, is preserved

 * information about each step of the sequence can be stored

 * data inside a pipeline can be accessed without breaking the chain

 * effects -- plotting, caching, writing -- can be inside the chain

 * errors can be handled succinctly, without resort to the tryCatch

 * the pipelines can branch freely


# Installation

To build the package

```R
library(devtools)
install_github("arendsee/rmonad", build_vignette=TRUE)
library(rmonad)
```

To build, install, and view the vignettes

```R
vignette("intro", package="rmonad")
```

# How to use Rmonad


It is easiest to show using a few examples

```R
read.table("asdfdf") %$>% colSums %>>% mean
```

The `%$>%` operator loads the left-hand side into the monad, handling any
errors, warnings or messages in a pure fashion. The result is then "bound" to
`colSums`. If the left hand side passed without error, its pure value will be
passed to `colSums`. If it failed, `colSums`, will not be called, and the
failed state will propagate.

This pipeline is pure in that there are no side-effects (e.g. nothing is
written to `stderr`).

If `read.table("asdfdf")` passes but it contains character columns, `colSums`
will fail. This failure will be recorded and propagated.

```R
r1 <- read.table("asdfdf") %$>% colSums %>>% mean  # dies on read.table
r2 <- iris %$>% colSums %>>% mean                  # dies on colSums 
r3 <- cars %$>% colSums %>>% mean                  # passes
```

Though the first two fail, the failure is contained within the monad. So the
exact location of the failure can be explored.
