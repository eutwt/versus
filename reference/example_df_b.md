# Modified version of `datasets::mtcars` - version b

A version of mtcars with some values altered and some rows/columns
removed. Not for informational purposes, used only to demonstrate the
comparison of two slightly different data frames. Since some values were
altered at random, the values do not necessarily reflect the true
original values. The variables are as follows:

## Usage

``` r
example_df_b
```

## Format

A data frame with 9 rows and 9 variables:

- car:

  The rowname in the corresponding
  [`datasets::mtcars`](https://rdrr.io/r/datasets/mtcars.html) row

- wt:

  Weight (1000 lbs)

- mpg:

  Miles/(US) gallon

- hp:

  Gross horsepower

- cyl:

  Number of cylinders

- disp:

  Displacement (cu.in.)

- carb:

  Number of carburetors

- drat:

  Rear axle ratio

- vs:

  Engine (0 = V-shaped, 1 = straight)

## Source

Sourced from the CRAN datasets package, with modified values. Originally
from Henderson and Velleman (1981), Building multiple regression models
interactively. *Biometrics*, **37**, 391–411.
