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
  funnel(x, f=foo, b=bar) %*>%
  (function(x,f,b){

     "This definitely won't work"
     
     b - f - x - f
  })
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
    sysinfo = session_info()
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
#> [1] "\n```{r, eval=FALSE}\n{\n    \"# Report\\n\\n  This is a pipeline report\\n  \"\n}\n```\n\nthis is a docstring\n```{r, eval=FALSE}\n{\n    5\n}\n```\n\nthis is too\n```{r, eval=FALSE}\n{\n    sqrt(.)\n}\n```\n\n# Conclusion\n\n   optional closing remarks\n   \n```{r, eval=FALSE}\n{\n    NULL\n}\n```\n"
```

## Contributing

I am looking for collaborators. There are enough unsolved problems on the
function graph side of rmonad to easily merit coauthorship. Similarly for the
report generation handling. In addition, there are lots of smaller problems.
See the next section for a partial summary.

## rmonad v0.2.0 goals

Here are my goals for the v0.2.0 release.

 - [x] Add optional docstrings to anonymous functions 

 - [ ] Cleanup the bind function (it is a bloated mess)

 - [ ] Extend test coverage to near 100%

 - [ ] Benchmark rmonad, speed it up as much as is reasonable

 - [ ] Improve documentation

   - [ ] Detailed documentation for each infix operator

 - [ ] Better examples (my current examples are highly contrived)

   - [ ] A monad theory vignette

   - [ ] One of two detailed case study vignettes

   - [ ] Improved examples in the introduction vignette and README

 - [ ] Reimplement the underlying graph structure with igraph (or some similar
   dedicated graph library). This is a major task. Currently `rmonad` builds
   directed graphs, but can do little with them. This should allow:

   - [ ] Plotting of the function graph

   - [ ] Faster, more elegant traversal algorithms

   - [ ] Preserve all operations in the graph, even those that were not run
     or that trail after failed states.

   - [ ] Find a way to deal with lists of graphs

   - [ ] Check for infinite loops

 - [ ] Functions for converting the pipeline data structure into an Rmd. There
   are a lot of details to work out here.

   - [ ] Set docstring to caption if the chunk emits a graph

   - [ ] Else make it a block of text underneath the chunk (or something)

   - [ ] ...

 - [ ] Add some system for caching, a global option system perhaps

 - [ ] Add some system for job submission. The monad controls evaluation, it
   should be possible to just swap out the evaluation function inside the bind
   function to evaluate in a different setting (e.g. a cluster). But I have
   little experience in this area.

 - [ ] Add a system parameterizing individual chunks. A modification of the
   current docstring system, but with a list, instead of a string. This could
   specify a function for caching, or runtime environment, or arguments to pass
   on to knitr.
