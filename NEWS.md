# rmonad 0.2.0

## Backwards incompatible changes

 * Rename `lsmeval` as `funnel`.

 * `combine` now works exclusively on monadic lists

 * Allow nesting of Rmonads, `as_monad` no longer automatically unnests them,
   there is an explicit function for this, `unnest`.

 * `as_monad` now records time and space (previously were left as NA)

## New features

 * Generation of Markdown reports from pipelines with `mreport`

 * Convert pipeline to DiagrammeR graph with `as_dgr_graph`.

 * Docstrings and metadata in anonymous functions

 * Support for multivariate anonymous functions

# Fixes

 * Allow `%*>%` to take monad bound lists

## Minor

 * Update README

 * Put docstring first in the printed output 

 * Add GFF case study vignette

 * Add `is_rmonad` function



# rmonad 0.1.0

 * Initial release
