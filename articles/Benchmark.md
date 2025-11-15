# Benchmark

The output of
[`versus::compare()`](https://eutwt.github.io/versus/reference/compare.md)
is designed to contain only the minimal amount of information needed to
represent the difference between two data frames: rows with value
differences, unmatched columns, and unmatched rows. While other packages
offer much more functionality, the minimal approach makes versus much
faster.

Below is an example benchmark using data from the `nycflights13` package
over-sampled to various sizes. In the example data, 5% of rows from each
data frame are missing in the other, and in 4 out of the 15 columns, 5%
of values are different.

At 1 million rows, versus takes under half a second and half a gigabyte
of memory, while others take over 10 seconds and multiple gigabytes of
memory. This is made possible by efficient functions from `vctrs` and
`collapse`. The
[`compare()`](https://eutwt.github.io/versus/reference/compare.md)
operation is roughly described by the steps below.

- Locate matching rows between `table_a` and `table_b` with
  [`vctrs::vec_locate_matches`](https://vctrs.r-lib.org/reference/vec_locate_matches.html)
- Subset tables according to match indices using
  [`collapse::ss`](https://sebkrantz.github.io/collapse/reference/fsubset.html)
- Use `collapse::%!=%` to identify differing values

### Benchmark code:

``` r
suppressPackageStartupMessages(library(dplyr))

generate_example_data <- function(n_rows) {
  tbl <- nycflights13::weather |>
    slice_sample(n = n_rows, replace = TRUE) |>
    # simulate unique key
    mutate(time_hour = Sys.time() + row_number())

  tbl_a <- tbl |>
    slice_sample(n = floor(n_rows * 0.95))
  tbl_b <- tbl |>
    slice_sample(n = floor(n_rows * 0.95)) |>
    # simulate value differences
    mutate(across(temp:wind_dir, \(x) ifelse(runif(n()) < .05, x + 1, x)))
  list(a = tbl_a, b = tbl_b)
}

bench_out <- bench::press(
  n_rows = c(1e5, 1e6, 2e6),
  {
    tbl <- generate_example_data(n_rows)
    bench::mark(
      versus =
        versus::compare(tbl$a, tbl$b, by = c(origin, time_hour)),
      arsenal =
        arsenal::comparedf(tbl$a, tbl$b, by = c("origin", "time_hour")),
      dataCompareR =
        dataCompareR::rCompare(tbl$a, tbl$b, keys = c("origin", "time_hour")),
      diffdf =
        diffdf::diffdf(tbl$a, tbl$b, keys = c("origin", "time_hour"), suppress_warnings = TRUE),
      min_iterations = 2,
      max_iterations = 2,
      check = FALSE
    )
  }
)
```

### Benchmark results:

``` r
# benchmark run on 2020 i7 MBP with 32GB of memory
bench_out |>
  summary() |>
  mutate(across(n_rows, scales::comma)) |>
  select(1:6) |>
  print(n = Inf)
#> # A tibble: 12 × 6
#>    expression   n_rows         min   median `itr/sec` mem_alloc
#>    <bch:expr>   <chr>     <bch:tm> <bch:tm>     <dbl> <bch:byt>
#>  1 versus       100,000    41.42ms  61.31ms  16.3       40.08MB
#>  2 arsenal      100,000   641.63ms 668.55ms   1.50        389MB
#>  3 dataCompareR 100,000      1.09s    1.18s   0.848    265.39MB
#>  4 diffdf       100,000      3.45s    3.54s   0.283    554.95MB
#>  5 versus       1,000,000 383.67ms 386.76ms   2.59     403.69MB
#>  6 arsenal      1,000,000   11.63s   11.67s   0.0857     3.77GB
#>  7 dataCompareR 1,000,000   15.99s   16.44s   0.0608     2.63GB
#>  8 diffdf       1,000,000   54.96s   55.04s   0.0182     5.35GB
#>  9 versus       2,000,000    1.16s     1.2s   0.833    806.13MB
#> 10 arsenal      2,000,000   24.05s   26.24s   0.0381     7.54GB
#> 11 dataCompareR 2,000,000   38.77s   39.85s   0.0251     5.27GB
#> 12 diffdf       2,000,000    1.89m    1.97m   0.00847    10.7GB
```
