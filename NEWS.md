# cheese 0.1.1

Fixed a bug that caused the wrong ordering of columns in the result of `univariate_table` when there was more than nine (9) columns

# cheese 0.1.0

## Changes to existing functions

* `absorb`

  - The `print` argument was renamed to `trace`

* `descriptives`

  - The `na.rm` and `useNA` arguments are no longer required for additional functions
  - Added `na_string` argument to for filling in `NA` result names
  - Removed leading dots (e.g. `.value`) from result columns
  - Added dedicated columns for the data type a function was evaluated for, and the names of a result
  - Function, column, and result attributes have `fun_`, `col_`, and `val_` prefixes, respectively

* `divide`

  - Default behavior is now to split to a multi-level list depending on the number of split variables
  - Control over the depth at which the list is split to
  - Select split variables unquoted, quoted, or with select helpers as in `dplyr::select()`
  - Control over whether unused split combinations are dropped with the `drop` argument
  - Control over whether stratification variables are removed from the split frames with the `remove` argument
  
* `stratiply`

  - User is in charge of binding back together (e.g. with `fasten`)

* `stretch`

  - The `keep` argument is removed
  - The `keys` argument is replaced with `key` and `send` argument replaced with `value`
  - Extracting keys from the header is now done with `grable()`.

* `typly`

  - Removed the `keep` argument

* `type_match`

  - Changed function name to `some_type`

* `univariate_table`

  - Default access to 25th and 75th percentiles for string templates with `"q1"` and `"q3"`, respectively
  - Uses `"median (q1, q3)"` as the default string template for numeric variables
  - Columns identified as `other` are not displayed by default
  - Stratification variables are displayed as a hierarchy, and the sample size is always shown for the lowest group
  - Row stratification variables are displayed consistently across rendering types to maintain headers

## New functions

* `depths` finds the unique depth(s) of elements in a list that satisfy a predicate
* `depths_string` finds paths and locations of elements that satisfy a predicate
* `fasten` takes a divided data frame and binds it back together
* `grable` makes a `knitr::kable` with stacked headers
* `muddle` randomly permutes some or all of the columns of a data frame

# cheese 0.0.3

Minor update to documentation

# cheese 0.0.2

Minor update to documentation

# cheese 0.0.1

Initial CRAN release