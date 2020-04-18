
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
  dplyr::select_if(is.numeric) %>%
  univariate_table(
    format = format,
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

| Variable    | Median (Q1, Q3)  | Mean (SD)      | \# missing |
| :---------- | :--------------- | :------------- | :--------- |
| Age         | 56 (48, 61)      | 54.44 (9.04)   | 0 of 303   |
| BP          | 130 (120, 140)   | 131.69 (17.6)  | 0 of 303   |
| Cholesterol | 241 (211, 275)   | 246.69 (51.78) | 0 of 303   |
| MaximumHR   | 153 (133.5, 166) | 149.61 (22.88) | 0 of 303   |

## General functions

``` r
#Run stratified multiple regression models for multiple outcomes
models <-
  heart_disease %>%
  
  #Within each sex
  stratiply(
    by = Sex,
    f =
      ~.x %>%
      
      #Regress some outcomes on all other variables
      dish(
        left = c(ExerciseInducedAngina, HeartDisease),
        right = -c(ChestPain, ExerciseInducedAngina, HeartDisease),
        each_right = FALSE,
        f = function(y, x) glm(y ~ ., data = x, family = "binomial")
      )
  )

summary(models$Female$HeartDisease)
#> 
#> Call:
#> glm(formula = y ~ ., family = "binomial", data = x)
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -1.7077  -0.7092  -0.4425   0.2726   2.5685  
#> 
#> Coefficients:
#>                 Estimate Std. Error z value Pr(>|z|)   
#> (Intercept)    -5.065838   3.420928  -1.481  0.13865   
#> Age             0.001257   0.036131   0.035  0.97225   
#> BP              0.052462   0.017342   3.025  0.00248 **
#> Cholesterol     0.004080   0.004231   0.964  0.33496   
#> BloodSugarTRUE  0.566391   0.721140   0.785  0.43221   
#> MaximumHR      -0.029809   0.015049  -1.981  0.04762 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 110.710  on 96  degrees of freedom
#> Residual deviance:  87.757  on 91  degrees of freedom
#> AIC: 99.757
#> 
#> Number of Fisher Scoring iterations: 5

#Find the depths of the list structure that are glm's
model_depth <-
  models %>%
    depths(
      predicate = some_type,
      types = "glm"
    )
model_depth
#> [1] 2

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
    into = "Outcome",
    depth = 1
  )
model_effects
#> $Female
#> # A tibble: 12 x 3
#>    Outcome               Term           Estimate
#>    <chr>                 <chr>             <dbl>
#>  1 ExerciseInducedAngina (Intercept)    -2.05   
#>  2 ExerciseInducedAngina Age            -0.0549 
#>  3 ExerciseInducedAngina BP              0.0442 
#>  4 ExerciseInducedAngina Cholesterol     0.00378
#>  5 ExerciseInducedAngina BloodSugarTRUE  0.497  
#>  6 ExerciseInducedAngina MaximumHR      -0.0215 
#>  7 HeartDisease          (Intercept)    -5.07   
#>  8 HeartDisease          Age             0.00126
#>  9 HeartDisease          BP              0.0525 
#> 10 HeartDisease          Cholesterol     0.00408
#> 11 HeartDisease          BloodSugarTRUE  0.566  
#> 12 HeartDisease          MaximumHR      -0.0298 
#> 
#> $Male
#> # A tibble: 12 x 3
#>    Outcome               Term           Estimate
#>    <chr>                 <chr>             <dbl>
#>  1 ExerciseInducedAngina (Intercept)     6.81   
#>  2 ExerciseInducedAngina Age            -0.00766
#>  3 ExerciseInducedAngina BP             -0.00865
#>  4 ExerciseInducedAngina Cholesterol     0.00555
#>  5 ExerciseInducedAngina BloodSugarTRUE  0.0604 
#>  6 ExerciseInducedAngina MaximumHR      -0.0486 
#>  7 HeartDisease          (Intercept)     4.00   
#>  8 HeartDisease          Age             0.0220 
#>  9 HeartDisease          BP              0.00578
#> 10 HeartDisease          Cholesterol     0.00923
#> 11 HeartDisease          BloodSugarTRUE -0.325  
#> 12 HeartDisease          MaximumHR      -0.0521
```
