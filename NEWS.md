# cheeese 0.1.0

_This version is in development_

## Changes to existing functions

* `absorb`

  - If there are duplicate keys, only the value from the first is used (unless the pattern is found again)
  - User is in charge of evaluating expressions from the resulting strings

* `divide`

  - Default behavior is now to split to a multi-level list depending on the number of split variables
  - Control over the depth at which the list is split to
  - Select split variables unquoted, quoted, or with select helpers as in `dplyr::select`
  - Control over whether unused split combinations are dropped with the `drop` argument
  - Control over whether stratification variables are removed from the split frames with the `remove` argument
  
* `stratiply`

  - User is in charge of binding back together (e.g. with `fasten`)

* `stretch` (removed)

  - Use `tidyr::pivot_wider` (starting version 1.0.0) for this functionality
  - Use `grable` to create a `knitr::kable` with stacked headers

* `type_match` and `typly` (removed)

  -Removed due to other existing functions serving their purpose

* `univariate_table`

  - Default access to 25th and 75th percentiles for string templates with `"q1"` and `"q3"`, respectively
  - Use `"median (q1, q3)"` as the default string template for numeric variables

## New functions

* `depths` finds the depth(s) of elements that satisfy a predicate
* `depths_string` finds paths and locations of elements that satisfy a predicate
* `fasten` takes a divided data frame and merges it back together
* `grable` makes a `knitr::kable` with stacked headers
* `muddle` randomly permutes some or all of the columns of a data frame

# cheese 0.0.3

Minor update to documentation

# cheese 0.0.2

Minor update to documentation

# cheese 0.0.1

Initial CRAN release