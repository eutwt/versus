# Modified version of `datasets::mtcars` - version a

A version of mtcars with some values altered and some rows/columns
removed. Not for informational purposes, used only to demonstrate the
comparison of two slightly different data frames. Since some values were
altered at random, the values do not necessarily reflect the true
original values. The variables are as follows:

## Usage

``` r
example_df_a
```

## Format

A data frame with 9 rows and 9 variables:

- car:

  The rowname in the corresponding
  [`datasets::mtcars`](https://rdrr.io/r/datasets/mtcars.html) row

- mpg:

  Miles/(US) gallon

- cyl:

  Number of cylinders

- disp:

  Displacement (cu.in.)

- hp:

  Gross horsepower

- drat:

  Rear axle ratio

- wt:

  Weight (1000 lbs)

- vs:

  Engine (0 = V-shaped, 1 = straight)

- am:

  Transmission (0 = automatic, 1 = manual)

## Source

Sourced from the CRAN datasets package, with modified values. Originally
from Henderson and Velleman (1981), Building multiple regression models
interactively. *Biometrics*, **37**, 391–411.
