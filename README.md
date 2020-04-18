
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/cheese.jpg" width="200" />

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/cheese)](https://cran.r-project.org/package=cheese)
![CRAN\_Download\_Counter](http://cranlogs.r-pkg.org/badges/grand-total/cheese)
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
concept (i.e. `purrr`) and use non-standard evaluation for column
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

  - From CRAN (current version 0.0.3)

`install.packages("cheese")`

  - From source (in development)

`devtools::install_github("zajichek/cheese")`

# Examples

## Creating a table of descriptive statistics

``` r
#Load package
require(cheese)
#> Loading required package: cheese

#Set a render format
format <- "markdown" #Could be "html" (default), "latex", "pandoc", or "none"

#Default
heart_disease %>%
  univariate_table(
    format = format
  )
```

| Variable              | Level            | Summary          |
| :-------------------- | :--------------- | :--------------- |
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
#Use formulas to stratify 
heart_disease %>%
  univariate_table(
    format = format,
    strata = ~ Sex,
    add_n = TRUE
  )
```

| Variable              | Level            | Female (N=97)  | Male (N=206)        |
| :-------------------- | :--------------- | :------------- | :------------------ |
| Age                   |                  | 57 (50, 63)    | 54.5 (47, 59.75)    |
| ChestPain             | Typical angina   | 4 (4.12%)      | 19 (9.22%)          |
|                       | Atypical angina  | 18 (18.56%)    | 32 (15.53%)         |
|                       | Non-anginal pain | 35 (36.08%)    | 51 (24.76%)         |
|                       | Asymptomatic     | 40 (41.24%)    | 104 (50.49%)        |
| BP                    |                  | 132 (120, 140) | 130 (120, 140)      |
| Cholesterol           |                  | 254 (215, 302) | 235 (208.75, 268.5) |
| MaximumHR             |                  | 157 (142, 165) | 150.5 (132, 167.5)  |
| ExerciseInducedAngina | No               | 75 (77.32%)    | 129 (62.62%)        |
|                       | Yes              | 22 (22.68%)    | 77 (37.38%)         |
| HeartDisease          | No               | 72 (74.23%)    | 92 (44.66%)         |
|                       | Yes              | 25 (25.77%)    | 114 (55.34%)        |

``` r
#Use string templates to customize cell appearance
heart_disease %>%
  univariate_table(
    format = format,
    categorical_summary = 
      c(
        `Count (%)` = "count (percent%)"
      ),
    numeric_summary = 
      c(
        `Median (Q1, Q3)` = "median (q1, q3)",
        `Mean (SD)` = "mean (sd)"
      ),
    all_summary = 
      c(
        `# missing` = "missing of length"
      )
  )
```

| Variable              | Level            | Median (Q1, Q3)  | Mean (SD)      | \# missing | Count (%)    |
| :-------------------- | :--------------- | :--------------- | :------------- | :--------- | :----------- |
| Age                   |                  | 56 (48, 61)      | 54.44 (9.04)   | 0 of 303   |              |
| Sex                   |                  |                  |                | 0 of 303   |              |
|                       | Female           |                  |                |            | 97 (32.01%)  |
|                       | Male             |                  |                |            | 206 (67.99%) |
| ChestPain             |                  |                  |                | 0 of 303   |              |
|                       | Typical angina   |                  |                |            | 23 (7.59%)   |
|                       | Atypical angina  |                  |                |            | 50 (16.5%)   |
|                       | Non-anginal pain |                  |                |            | 86 (28.38%)  |
|                       | Asymptomatic     |                  |                |            | 144 (47.52%) |
| BP                    |                  | 130 (120, 140)   | 131.69 (17.6)  | 0 of 303   |              |
| Cholesterol           |                  | 241 (211, 275)   | 246.69 (51.78) | 0 of 303   |              |
| BloodSugar            |                  |                  |                | 0 of 303   |              |
| MaximumHR             |                  | 153 (133.5, 166) | 149.61 (22.88) | 0 of 303   |              |
| ExerciseInducedAngina |                  |                  |                | 0 of 303   |              |
|                       | No               |                  |                |            | 204 (67.33%) |
|                       | Yes              |                  |                |            | 99 (32.67%)  |
| HeartDisease          |                  |                  |                | 0 of 303   |              |
|                       | No               |                  |                |            | 164 (54.13%) |
|                       | Yes              |                  |                |            | 139 (45.87%) |

## General functions

``` r
#Run stratified univariate regression models for multiple outcomes
models <-
  heart_disease %>%
  
  #Within each sex
  stratiply(
    by = Sex,
    f =
      ~.x %>%
      
      #Regress some outcomes on all other variables individually
      dish(
        left = c(ExerciseInducedAngina, HeartDisease),
        f = function(y, x) glm(y ~ x, family = "binomial")
      )
  )

summary(models$Female$HeartDisease$Age)
#> 
#> Call:
#> glm(formula = y ~ x, family = "binomial")
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -1.1870  -0.8324  -0.6500   1.4148   1.9950  
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)   
#> (Intercept) -4.27420    1.62356  -2.633  0.00847 **
#> x            0.05654    0.02769   2.042  0.04118 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 110.71  on 96  degrees of freedom
#> Residual deviance: 106.13  on 95  degrees of freedom
#> AIC: 110.13
#> 
#> Number of Fisher Scoring iterations: 4

#Find the depths of the list structure that are glm's
model_depth <-
  models %>%
    depths(
      predicate = some_type,
      types = "glm"
    )
model_depth
#> [1] 3

#Gather model effect estimates
model_effects <-
  models %>%
  
  #For each model
  purrr::map_depth(
    .depth = model_depth,
    ~.x %>%
      
      #Collect model coefficients into a tibble
      purrr::pluck("coefficients") %>%
      tibble::enframe(
        name = "Term",
        value = "Estimate"
      )
  ) %>%
  
  #Bind rows up to the stratification variable
  fasten(
    into = c("Outcome", "Predictor"),
    depth = 1
  )
model_effects
#> $Female
#> # A tibble: 28 x 4
#>    Outcome               Predictor   Term               Estimate
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
#> # … with 18 more rows
#> 
#> $Male
#> # A tibble: 28 x 4
#>    Outcome               Predictor   Term              Estimate
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
#> # … with 18 more rows
```
