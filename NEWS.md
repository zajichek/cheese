# cheese 0.0.2

_This version is in development_

## Changes to existing functions

* `divide`

  - Default behavior is now to split to a multi-level list depending on the number of split variables
  - Control over the depth at which the list is split to
  - Select split variables unquoted, quoted, or with select helpers as in `dplyr::select`
  - Control over whether unused split combinations are dropped with the `drop` argument
  - Control over whether stratification variables are removed from the split frames with the `remove` argument
  
* `stratiply`

  - Retain original stratification columns by default when result is binded
  - When result is unbinded, the depth reflects that of `divide`

* `stretch`

  - Works when there are no `keep` variables
  - Variables spread over the columns are right-most in the result

* `univariate_table`

  - Default access to 25th and 75th percentiles for string templates with `"q1"` and `"q3"`, respectively
  - Use `"median (q1, q3)"` as the default string template for numeric variables

## New functions

* `chop` removes or extracts a vector of patterns from the beginning or end of a vector of strings
* `depths` traverses a list structure to find elements that satisfy a predicate
* `explore` using flexible models to visually assess non-linear relationships and interactions
* `fasten` takes a divided data frame and merges it back together
* `muddle` randomly permutes some or all of the columns of a data frame
* `pick` selects a subset of a list as in `dplyr::select`
* `regression_table` creates a custom table for regression models similar to `univariate_table`
* `wander` reduces a set of columns from a `data.frame` by user-specified functions, stopping criteria, comparator values, etc.

# cheese 0.0.1

Initial CRAN release