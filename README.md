[![Travis-CI Build Status](https://travis-ci.org/arendsee/rmonad.svg?branch=dev)](https://travis-ci.org/arendsee/rmonad)
[![Coverage Status](https://img.shields.io/codecov/c/github/arendsee/rmonad/dev.svg)](https://codecov.io/github/arendsee/rmonad?branch=dev)

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
funnel(
    runif(5),
    stop("stop, drop and die"),
    runif("df"),
    1:10
)
#> R> "1:10"
#> R> "runif("df")"
#>  * ERROR: invalid arguments
#>  * WARNING: NAs introduced by coercion
#> R> "stop("stop, drop and die")"
#>  * ERROR: stop, drop and die
#> R> "runif(5)"
#> R> "funnel(runif(5), stop("stop, drop and die"), runif("df"), 1:10)"
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
funnel(
    read.csv("a.csv") %>>% do_analysis_a,
    read.csv("b.csv") %>>% do_analysis_b,
    k = 5
) %*>% joint_analysis
```


```r
foo <- {

  "This is nothing"

  NA

} %>>% {

  "This the length of nothing"

  length(.) 
}

bar <- {

  "These are cars"

  cars

} %>>% {

  "There are this many of them"

  length(.)
}


baz <- "oz" %>>%
  funnel(f=foo, b=bar) %*>%
  {

     "This definitely won't work"
     
     . + f + b
  }
```


### Chain independent pipelines, with documentation


```r
analysis <- 
{
    "This analysis begins with 5 uniform random variables"

    runif(5)

} %>>% '^'(2) %>>% sum %__%
{
    "The next step is to take 6 normal random variables"

    rnorm(6)  
} %>>% '^'(2) %>>% sum %v__%
{
    "And this is were the magic happens, we take 'a' random normal variables"

    rnorm("a")

} %>>% '^'(2) %>>% sum %__%
{
    "Then, just for good measure, we toss in six exponentials"

    rexp(6)

} %>>% '^'(2) %>>% sum

analysis
#> 
#> 
#>     This analysis begins with 5 uniform random variables
#> 
#> R> "{
#>     runif(5)
#> }"
#> R> "`^`(2)"
#> R> "sum"
#> 
#> 
#>     The next step is to take 6 normal random variables
#> 
#> R> "{
#>     rnorm(6)
#> }"
#> R> "`^`(2)"
#> R> "sum"
#> [1] 10.78472
#> 
#> 
#> 
#>     And this is were the magic happens, we take 'a' random normal variables
#> 
#> R> "{
#>     rnorm("a")
#> }"
#>  * ERROR: invalid arguments
#>  * WARNING: NAs introduced by coercion
#> 
#> 
#>     Then, just for good measure, we toss in six exponentials
#> 
#> R> "{
#>     rexp(6)
#> }"
#> R> "`^`(2)"
#> R> "sum"
#> 
#>  ----------------- 
#> 
#> [1] 29.97433
```

### Add metadata to chunk


```r
{
  "This is data describing a chunk"

  list(
    foo = "this is metadata, you can put anything you want in here",
    bar = "maybe can pass parameters to an Rmarkdown chunk",
    baz = "or store stuff in state, for example:"
    sysinfo = devtools::session_info()
  )

  # this is the actual thing computed
  1 + 1
}
```

### Build Markdown report from a pipeline

Handling for this is currently extremely rudimentary. The `mreport` function
will concatenate all the code run in the pipeline along with the docstrings.


```r
x <- 
{
  "# Report

  This is a pipeline report
  "

} %__% {
  
  "this is a docstring"
  
  5

} %>>% {
  
  "this is too"
  
  sqrt(.)

} %>_% {

   "# Conclusion

   optional closing remarks
   "

  NULL

}
mreport(x)
#> [1] "## 1\n   # Conclusion\n\n   optional closing remarks\n   \n```{r, eval=FALSE}\n{\n    \"# Report\\n\\n  This is a pipeline report\\n  \"\n}\n```\nOK=TRUE | nparents=0 | nbranches=0 | cached=FALSE\n\n## 2\n   # Conclusion\n\n   optional closing remarks\n   \n```{r, eval=FALSE}\n{\n    5\n}\n```\nOK=TRUE | nparents=1 | nbranches=0 | cached=FALSE\n\n## 3\n   # Conclusion\n\n   optional closing remarks\n   \n```{r, eval=FALSE}\nfunction (.) \n{\n    \"this is too\"\n    sqrt(.)\n}\n```\nOK=TRUE | nparents=1 | nbranches=0 | cached=FALSE\n\n## 4\n   # Conclusion\n\n   optional closing remarks\n   \n```{r, eval=FALSE}\nfunction (.) \n{\n    \"# Conclusion\\n\\n   optional closing remarks\\n   \"\n    NULL\n}\n```\nOK=TRUE | nparents=1 | nbranches=0 | cached=TRUE\n"
```

### Graphing pipelines


```r
# here I use the `->` operator, which is the little known twin of `<-`.
funnel(
  "a" %v>% paste("b"), # %v>% stores the input (%>>% doesn't)
  "c" %v>% paste("d")
) %*>% # %*>% bind argument list from funnel to paste
  paste %>%  # funnel joins monads, so we pass in the full monad here, with
  funnel(    # '%>%', rather than use '%>>'% to get the wrapped value
    "e" %v>% paste("f"),
    "g" %v>% paste("h")
  ) %*>%
  paste %>% # the remaining steps are all operating _on_ the monad
  plot(label='value')
#> Warning in .doLoadActions(where, attach): trying to execute load actions
#> without 'methods' package
```

![plot of chunk workflow-plot](README-workflow-plot-1.png)

Nested pipelines can also be plotted:


```r
foo <- function(x){
    'c' %v>% paste(x) %v>% paste('d')
}
'a' %v>% foo %>% plot(label='value')
```

![plot of chunk nested-workflow-plot](README-nested-workflow-plot-1.png)


## Contributing

I am looking for collaborators. There are enough unsolved problems on the
function graph side of rmonad to easily merit coauthorship. Similarly for the
report generation handling. In addition, there are lots of smaller problems.
See the next section for a partial summary.


## rmonad v0.2.0 goals

The coming Rmonad v0.2.0 should have:

 - [ ] Cleaner code

 - [ ] Better vignettes and documentation

   - [ ] Detailed documentation for all infix operators

   - [ ] A monad theory vignette

   - [x] One of two detailed case study vignettes

 - [x] optional docstrings for bound functions 

 - [x] optional metadata in function preambles

 - [x] generic plot functions that support nested pipelines

 - [ ] a more elegant data structure for representing the workflow graph

   - [ ] preserve all operations, even those not run

   - [ ] check for infinite loops

 - [x] functions for converting the pipeline data structure into an Rmd

 - [ ] a system for persistant caching of results

 - [ ] a system for job submission
