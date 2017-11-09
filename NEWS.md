# rmonad 0.4.0

## Miscellaneous

 * Update report 

 * Add rewriter functions that can reformat warning, error, and note message,
   or summarize the result, after an Rmonad is built.

 * Free values stored in the Rmonad objects combined with funnel

 * Evaluate metadata in the correct environment

 * export `size` function that returns the number of nodes in a workflow

 * Replace 'children field with 'dependents' (and the associated accessors
   `get_children` and `has_children`). The reason is the `transitive` and
   `nest` edges are both also variants parent/child relationships.

 * Do no export `unnest` (you don't need it)

 * Fix 

 * Completely remove deprecated `%v__%` operator and the `doc` and `lsmeval`
   functions.

 * Replace the whole "branch" thing with children. There isn't anything so
   branchy about them, they are just multiple children of a node in a directed
   graph.

 * Remove `unbranch`

 * Remove the `recurse_nests` option in `mtabulate` and `missues`

## Internal

 * Remove `recursive_set_nest_depth` function. This was super kludgy internal function.

## Vectorize accessors

 * The vectorized `ms_*` are renamed as `get_*`.

 * Fixed a lot of bugs and inconsistencies in the vectorized getters.

 * The `m_*` functions are now deprecated. They provided access to the "head"
   of the workflow, but this isn't really all that well defined, since `rmonad`
   supports branching.

 * Add `has_*` vectorized accessors for the existence of fields (e.g.
   `has_value`, `has_error`).

## use `igraph` as graph datastructure

 * remove `as_dgr_graph`

 * `Rmonad` is now an S4 class
 
   - The raw igraph object is stored in the `graph` slot.

   - The vertex attributes are stored in a list in the `data` slot.

 * Plot with igraph, now `...` is passed to igraph.plot

## add cache system

The default is to store data in memory (as before).

New cache related functions:

<!-- TODO: describe a little about how caching works, introduce CacheManager -->

 * `make_local_cacher` - build a cache function that stores data as Rdata
   objects in a specified folder.

 * `memory_cache` - cache a value in memory

 * `make_recacher` - build a function to reset an `Rmonad` object's cacher

 * `no_cache` - xxx

# rmonad 0.3.0

 * Deprecate the `%v__%` operator. Now the `%__%` operator always stores the
   final left-side result.

 * Fix linking to nested history.

 * Fix incorrect history linking bugs caused by the `%__%` operator
 
 * Preserve the inputs to failing nested nodes. Now if a nested node fails, the
   input to that node, and the input to every ascending node, is preserved.
   (like a traceback where the function arguments are saved).

 * Add a new kind of edge. The `%__%` associates two pipelines but does not
   pass data or propagate errors. This was previously treated as one kind of
   'parent' edge; now it is a 'prior' edge.

 * Add special plot handling for 'prior' edges.

# rmonad 0.2.0

## Backwards incompatible changes

 * Rename `lsmeval` as `funnel`

 * `combine` now works exclusively on lists of Rmonad objects

 * Allow nesting of Rmonads, `as_monad` no longer automatically unnests them

 * Deprecate the `doc` function. Instead use doc strings.

## New features

 * Docstrings and metadata in anonymous functions

 * Support for multivariate anonymous functions

 * `as_monad` now records time and space (previously were left as NA)

 * Rmonad class refactored as an R6 class

 * Allow `%*>%` to take monad bound lists

 * `m_value` raises a warning (by default) when accessing an uncached value.

## New functions

 * `mreport` - Generate of rudimentary Markdown reports

 * `as_dgr_graph` - Convert pipeline to DiagrammeR graph 

 * `is_rmonad` - Tests if something is an `Rmonad` object

 * `unnest` - remove a level of nesting from a node

 * `extract_metadata` - get docstring and metadata from a function or block

 * `m_nest` - get or set a node's nested pipeline

 * `m_nest_depth` - get or set a node's nest depth

 * `m_meta` - get or set a node's metadata

 * `m_id` - get or set a node's id (changing an existing id raises a warning)

 * `app_parents` - add a list of parents to a node

 * `m_delete_value` - delete the value stored by a node 

## Fixes

 * `NULL` can now be stored as a value and is distinguishable from Nothing
   (i.e. an uncached value).

 * as.list now lists elements in the expected order

 * Errors raised are stored even if they are not non-empty strings. Previously
   calls like `stop()` would be reconed as passing.

## Minor

 * Update README

 * Put docstring first in the printed output 

 * New vignettes

## New bugs

 * Nest level is not set at runtime, but rather set upon conversion to
   a DiagrammeR object. In the meantime, nest depth is NA.


# rmonad 0.1.0

 * Initial release
