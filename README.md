
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cheese <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/cheese)](https://cran.r-project.org/package=cheese)
![CRAN_Download_Counter](http://cranlogs.r-pkg.org/badges/grand-total/cheese)
<!-- badges: end -->

# Introduction

The `cheese` package contains tools for working with data during
statistical analysis–promoting flexible, intuitive, and reproducible
workflows. There are functions designated for specific statistical tasks
such as

- `univariate_table()`: To create a custom table of descriptive
  statistics for a dataset
- `univariate_associations()`: For computing pairwise association
  metrics for combinations of `predictors` and `responses`
- `descriptives()`: To compute descriptive statistics on columns of a
  dataset

These are built on a collection of data manipulation tools designed for
general use, many of which are motivated by the functional programming
concept (i.e. `purrr`) and use non-standard evaluation for column
selection as in `dplyr::select`. Here are a few:

- `depths()`: Find the depth(s) of elements in a list structure that
  satisfy a predicate
- `divide()` and `fasten()`: Split/bind data frames to/from any list
  depth
- `dish()`: Evaluate a function with pairwise combinations of columns
- `stratiply()`: Evaluate a function on subsets of a data frame
- `typly()`: Evaluate a function on columns that inherit at least one
  (or none) of the specified classes

# Installation

- From CRAN

`install.packages("cheese")`

- From source

`devtools::install_github("zajichek/cheese")`

# Usage

``` r
#Load package
require(cheese)
#> Loading required package: cheese

#Make a descriptive table
heart_disease %>%
  univariate_table(
    format = "markdown" #Could also render as "html", "latex", "pandoc", or "none"
  )
```

| Variable              | Level            | Summary          |
|:----------------------|:-----------------|:-----------------|
| Age                   |                  | 56 (48, 61)      |
| Sex                   | Female           | 97 (32.01%)      |
|                       | Male             | 206 (67.99%)     |
| ChestPain             | Typical angina   | 23 (7.59%)       |
|                       | Atypical angina  | 50 (16.5%)       |
|                       | Non-anginal pain | 86 (28.38%)      |
|                       | Asymptomatic     | 144 (47.52%)     |
| BP                    |                  | 130 (120, 140)   |
| Cholesterol           |                  | 241 (211, 275)   |
| MaximumHR             |                  | 153 (133.5, 166) |
| ExerciseInducedAngina | No               | 204 (67.33%)     |
|                       | Yes              | 99 (32.67%)      |
| HeartDisease          | No               | 164 (54.13%)     |
|                       | Yes              | 139 (45.87%)     |

``` r
#Run some models
heart_disease %>%

  #Apply a function to subsets of the data
  stratiply(
    by = Sex,
    f =
      ~.x %>%
      
      #Apply a function to pairwise combinations of columns
      dish(
        left = c(ExerciseInducedAngina, HeartDisease),
        f = function(y, x) glm(y ~ x, family = "binomial") %>% purrr::pluck("coefficients") %>% tibble::enframe()
      )
  ) %>%
    
    #Bind rows up to a specified depth
    fasten(
      into = c("Outcome", "Predictor"),
      depth = 1
    )
#> $Female
#> # A tibble: 28 × 4
#>    Outcome               Predictor   name                  value
#>    <chr>                 <chr>       <chr>                 <dbl>
#>  1 ExerciseInducedAngina Age         (Intercept)        -1.46   
#>  2 ExerciseInducedAngina Age         x                   0.00416
#>  3 ExerciseInducedAngina ChestPain   (Intercept)       -17.6    
#>  4 ExerciseInducedAngina ChestPain   xAtypical angina   15.5    
#>  5 ExerciseInducedAngina ChestPain   xNon-anginal pain  14.8    
#>  6 ExerciseInducedAngina ChestPain   xAsymptomatic      17.4    
#>  7 ExerciseInducedAngina BP          (Intercept)        -6.47   
#>  8 ExerciseInducedAngina BP          x                   0.0383 
#>  9 ExerciseInducedAngina Cholesterol (Intercept)        -2.06   
#> 10 ExerciseInducedAngina Cholesterol x                   0.00315
#> # ℹ 18 more rows
#> 
#> $Male
#> # A tibble: 28 × 4
#>    Outcome               Predictor   name                 value
#>    <chr>                 <chr>       <chr>                <dbl>
#>  1 ExerciseInducedAngina Age         (Intercept)       -2.44   
#>  2 ExerciseInducedAngina Age         x                  0.0356 
#>  3 ExerciseInducedAngina ChestPain   (Intercept)       -1.32   
#>  4 ExerciseInducedAngina ChestPain   xAtypical angina  -1.39   
#>  5 ExerciseInducedAngina ChestPain   xNon-anginal pain -0.219  
#>  6 ExerciseInducedAngina ChestPain   xAsymptomatic      1.71   
#>  7 ExerciseInducedAngina BP          (Intercept)        0.0385 
#>  8 ExerciseInducedAngina BP          x                 -0.00424
#>  9 ExerciseInducedAngina Cholesterol (Intercept)       -1.70   
#> 10 ExerciseInducedAngina Cholesterol x                  0.00494
#> # ℹ 18 more rows
```

See the package vignettes and documentation for more thorough examples.
