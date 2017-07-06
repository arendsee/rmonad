[![Travis-CI Build Status](https://travis-ci.org/arendsee/rmonad.svg?branch=master)](https://travis-ci.org/arendsee/rmonad)
[![Coverage Status](https://img.shields.io/codecov/c/github/arendsee/rmonad/master.svg)](https://codecov.io/github/arendsee/rmonad?branch=master)

# Rmonad

`Rmonad` offers

 * nuanced error handling

 * access to the intermediate results of a pipeline

 * effects -- e.g. plotting, caching -- within a pipeline

 * access to results preceding an error

 * tools for branching and combining pipelines

 * a way to almost completely avoid naming things

 * a structured approach to literate programming

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

# Examples

For details, see the vignette. Here are a few excerpts

```R
1:5      %>>%
    sqrt %v>% # record an intermediate value
    sqrt %>>%
    sqrt
```

```R
# Both plots and summarizes an input table
cars %>_% plot(xlab="index", ylab="value") %>>% summary
```

```R
x <- list()

# return first value in a list, otherwise return NULL
if(length(x) > 0) {
    x[[1]]
} else {
    NULL
}

# this does the same
x[[1]] %||% NULL %>% esc
```

```R
# chain independent pipelines together, with documentation
runif(10)  %>>% abs %>% doc(

    "Alternatively, the documentation could go into a text block below the code
in a knitr document. The advantage of having documentation here, is that it is
coupled unambiguously to the generating function. This is a monadic
interpretation of literate programming. These annotations, together with the
ability to chain chains of monads, allows whole complex workflows to be built,
with the results collated into a single monad. All errors propagate exactly as
errors should, only affecting downstream computations. The final monad can be
converted into a markdown document. A graph of functions can automatically be
built. Summaries of the locations of errors. The monad could be extended for
automated benchmarking."

                  ) %>^% sum %^__%
rnorm(10)  %>>% abs %>^% sum %^__%
rnorm("a") %>>% abs %>^% sum %^__%
rexp(10)   %>>% abs %>^% sum %>%
    unbranch
```
