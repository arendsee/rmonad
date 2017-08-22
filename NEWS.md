# rmonad 0.2.0

## Backwards incompatible changes

 * Rename `lsmeval` as `funnel`.

 * `combine` now works exclusively on monadic lists

 * Allow nesting of Rmonads, `as_monad` no longer automatically unnests them,
   there is an explicit function for this, `unnest`.

 * Deprecate the `doc` function. Instead use doc strings.

## New features

 * Generation of rudimentary Markdown reports from pipelines with `mreport`

 * Convert pipeline to DiagrammeR graph with `as_dgr_graph`.

 * Docstrings and metadata in anonymous functions

 * Support for multivariate anonymous functions

 * `as_monad` now records time and space (previously were left as NA)

 * Rmonad class refactored as an R6 class

## Fixes

 * Allow `%*>%` to take monad bound lists

 * `NULL` can now be stored as a value and is distinguishable from Nothing
   (i.e. an uncached value). `m_value` will raise a warning when accessing an
   uncached value. The warning can be turned off.

 * as.list now lists elements in the expected order

 * Errors raised are stored even if they are not non-empty strings. Previously
   calls like `stop()` would be reconed as passing.

## Minor

 * Update README

 * Put docstring first in the printed output 

 * Add GFF case study vignette

 * Add `is_rmonad` function

## New bugs

 * nest level is set at runtime, but rather set in batch on converstion to
   a DiagrammeR object. In the meantime, nest depth is set to NA


# rmonad 0.1.0

 * Initial release
