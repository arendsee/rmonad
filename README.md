[![Travis-CI Build Status](https://travis-ci.org/arendsee/rmonad.svg?branch=master)](https://travis-ci.org/arendsee/rmonad)
[![Coverage Status](https://img.shields.io/codecov/c/github/arendsee/rmonad/master.svg)](https://codecov.io/github/arendsee/rmonad?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rmonad)](https://cran.r-project.org/package=rmonad)

# `rmonad`

Chain monadic sequences into stateful, branching pipelines. As nodes in the
pipeline are run, they are merged into a graph of all past operations. The
resulting structure can be computed on to access not only the final results,
but also node documentation, intermediate data, performance stats, and any raised
messages, warnings or errors. `rmonad` intercepts all exceptions, which allows
for pure error handling.

`rmond` complements, rather than competes with non-monadic pipelines packages
such as `magrittr` or `pipeR`. These can be used to perform operations where
preservation of state is not desired. Also they are needed to operate on
monadic containers themselves.

## Installation

You can install from CRAN with:


```r
install.packages("rmonad")
```

You can install from the github dev branch with:


```r
# install.packages("devtools")
devtools::install_github("arendsee/rmonad", ref="dev")
```

## On obsolescence and collaboration

Some of the material in `master` is already slated for deprecation. See `dev`
for the newest material. `rmonad` is experimental at this point.  If you are
interested in collaborating, shoot me an email. Also see the `dev` README.

## Examples

For details, see the vignette. Here are a few excerpts


```r
library(rmonad)
```


### Record history and access inner values


```r
1:5      %>>%
    sqrt %v>% # record an intermediate value
    sqrt %>>%
    sqrt
#> R> "1:5"
#> R> "sqrt"
#> [1] 1.000000 1.414214 1.732051 2.000000 2.236068
#> 
#> R> "sqrt"
#> R> "sqrt"
#> 
#>  ----------------- 
#> 
#> [1] 1.000000 1.090508 1.147203 1.189207 1.222845
```


### Add effects inside a pipeline


```r
# Both plots and summarizes an input table
cars %>_% plot(xlab="index", ylab="value") %>>% summary
```


### Use first successful result


```r
x <- list()

# return first value in a list, otherwise return NULL
if(length(x) > 0) {
    x[[1]]
} else {
    NULL
}
#> NULL

# this does the same
x[[1]] %||% NULL %>% esc
#> NULL
```


### Independent evaluation of multiple expressions


```r
lsmeval(
    runif(5),
    stop("stop, drop and die"),
    runif("df"),
    1:10
)
#> Warning: 'lsmeval' is deprecated.
#> Use 'funnel' instead.
#> See help("Deprecated")
#> R> "1:10"
#> R> "runif("df")"
#>  * ERROR: invalid arguments
#>  * WARNING: NAs introduced by coercion
#> R> "stop("stop, drop and die")"
#>  * ERROR: stop, drop and die
#> R> "runif(5)"
#> R> "funnel(..1, ..2, ..3, ..4, keep_history = keep_history)"
#> 
#>  ----------------- 
#> 
#> [[1]]
#> [1] 0.5120101 0.8351271 0.8930770 0.4460601 0.2983039
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> NULL
#> 
#> [[4]]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#>  *** FAILURE ***
```


### Build branching pipelines


```r
lsmeval(
    read.csv("a.csv") %>>% do_analysis_a,
    read.csv("b.csv") %>>% do_analysis_b,
    k = 5
) %*>% joint_analysis
```


### Chain independent pipelines, with documentation


```r
runif(5) %>>% abs %>% doc(

    "Alternatively, the documentation could go into a text block below the code
    in a knitr document. The advantage of having documentation here, is that it
    is coupled unambiguously to the generating function. These annotations,
    together with the ability to chain chains of monads, allows whole complex
    workflows to be built, with the results collated into a single object. All
    errors propagate exactly as errors should, only affecting downstream
    computations. The final object can be converted into a markdown document
    and automatically generated function graphs."

                  ) %>^% sum %__%
rnorm(6)   %>>% abs %>^% sum %v__%
rnorm("a") %>>% abs %>^% sum %__%
rexp(6)    %>>% abs %>^% sum %T>%
  { print(mtabulate(.)) } %>% missues
#>                       code    OK cached  time space nbranch nnotes
#> 2                 runif(5)  TRUE  FALSE    NA    NA       0      0
#> 21 eval(expr, envir = env)  TRUE   TRUE 0.001    NA       0      0
#> 3                      sum  TRUE  FALSE 0.001    88       1      0
#> 4                 rnorm(6)  TRUE  FALSE    NA    88       0      0
#> 5  eval(expr, envir = env)  TRUE   TRUE 0.001    NA       0      0
#> 6                      sum  TRUE   TRUE 0.001    88       1      0
#> 7               rnorm("a") FALSE  FALSE    NA     0       0      0
#> 8                  rexp(6)  TRUE  FALSE    NA    88       0      0
#> 9  eval(expr, envir = env)  TRUE   TRUE 0.001    NA       0      0
#> 10                     sum  TRUE   TRUE 0.001    88       1      0
#>    nwarnings error doc
#> 2          0     0   0
#> 21         0     0   0
#> 3          0     0   0
#> 4          0     0   0
#> 5          0     0   0
#> 6          0     0   0
#> 7          1     1   0
#> 8          0     0   0
#> 9          0     0   0
#> 10         0     0   0
#>   id    type                      issue
#> 1  7   error          invalid arguments
#> 2  7 warning NAs introduced by coercion
```

## `dev` branch features

### Docstrings

This allows chunks of code to be annotated without the exta boilerplate of
`%>%doc(...`, that was used in the previous example.


```r
{

  "This is a docstring"

  1 

} %>>% {

  "The docstrings can be used to document specific chunks of code. It is a lot
  cleaner than piping the monad into the `doc` function."

  ( . + . ) * ( . + . )

} %>_% {

  "If you are interested in docstrings and the newer rmonad features, see the
  github dev branch"

  NULL

}
```
