# rmonad 0.4.0

## Conceptual changes

 * Replace the internal ad hoc graph system with 'igraph'

   - `Rmonad` is now an S4 class
 
   - The raw igraph object is stored in the `graph` slot.

   - The vertex attributes are stored in a list in the `data` slot.

 * Replace the concept of "branches" (e.g. the `%>^%` operator) with
   "children". These are just multiple children of a node in a directed graph.

 * Add a cache system

## New functions and arguments

 * Export `size` function that returns the number of nodes in a workflow.

 * The vectorized `ms_*` are renamed as `get_*`.

 * Add `has_*` vectorized accessors for the existence of fields (e.g.
   `has_value`, `has_error`).

 * Plot with igraph, now `...` is passed to igraph.plot

 * Cache functions, these new functions are somewhat experimental:

   - `make_local_cacher` - build a cache function that stores data as Rdata
     objects in a specified folder.

   - `memory_cache` - cache a value in memory

   - `make_recacher` - build a function to reset an `Rmonad` object's cacher

   - `no_cache` - xxx

## Deprecated/internalized functions

 * Remove `unbranch`

 * Remove `as_dgr_graph`, now the underlying graph is already an igraph object,
   which can be converted easily to a DiagrammeR graph, if desired

 * Remove all the `ms_*` functions

 * Remove all `m_*` functions. They provided access to the "head" of the
   workflow, but this isn't really all that well defined, since `rmonad`
   supports branching.

 * Completely remove deprecated `%v__%` operator and the `doc` and `lsmeval`
   functions.

 * Do no export `unnest` (you don't need it)

## Bug fixes

 * Evaluate metadata in the correct environment

 * Now `nest_depth` is set as the Rmonad grows (in the past it wasn't added
   until mtabulate was run).

 * Free values stored in the Rmonad objects when they are combined with
   `funnel`, previously these values were stored even when they should have
   been removed.

 * Fixed a lot of bugs and inconsistencies in the vectorized getters.

## Other

 * Rename `mreport` as `report`. The content of the report is updated somewhat,
   but still should be considered as a template.

 * Add rewriter functions that can reformat warning, error, and note message,
   or summarize the result, after an Rmonad is built.

 * Replace 'children' field with 'dependents' (and the associated accessors
   `get_children` and `has_children`). The reason is the `transitive` and
   `nest` edges are both also variants parent/child relationships.

 * Deprecate the `recurse_nests` option in `mtabulate` and `missues`

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
