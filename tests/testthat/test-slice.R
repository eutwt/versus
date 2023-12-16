# slice_diffs() -----

test_that("slice_diffs_both works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(out)
})

test_that("slice_diffs_both works with multi-variable `by`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(out)
})

test_that("Error when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(
    example_df_a |> slice_diffs(disp),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> slice_diffs(example_df_b, disp),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> slice_diffs_both(example_df_b, disp),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> slice_diffs_both(example_df_b, 1, disp),
    error = TRUE
  )
})

test_that("Error when supplied table doesn't contain cols in `comparison`", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(slice_diffs(tibble(x = 1), comp, column = drat), error = TRUE)
  expect_snapshot(
    slice_diffs_both(test_df_a, tibble(car = 1), comp, column = drat),
    error = TRUE
  )
})

test_that("Error when `by` columns in `table` aren't compatible with `comparison`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  bad_table <- test_df_a %>% mutate(vs = as.character(vs))
  expect_snapshot(slice_diffs(bad_table, comp, column = mpg), error = TRUE)
})

test_that("slice_diffs works when there are no diffs", {
  df <- rownames_to_column(mtcars, "car")
  comp <- compare(df, df, by = "car")
  expect_identical(slice_diffs(df, comp), as_tibble(df[0, ]))
})


# slice_unmatched() -----

test_that("slice_unmatched_both works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  out <- slice_unmatched_both(test_df_a, test_df_b, comp)
  expect_snapshot(out)
})

test_that("slice_unmatched_both works with multi-variable `by`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  out <- slice_unmatched_both(test_df_a, test_df_b, comp)
  expect_snapshot(out)
})

test_that("unmatched() errors when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(
    example_df_a |> slice_unmatched(disp),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> slice_unmatched(example_df_b),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> slice_unmatched_both(example_df_b, disp),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> slice_unmatched_both(example_df_b, 1),
    error = TRUE
  )
})

test_that("unmatched() errors when supplied table doesn't contain cols in `comparison`", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(slice_unmatched(tibble(x = 1), comp), error = TRUE)
  expect_snapshot(
    slice_unmatched_both(test_df_a, tibble(x = 1), comp),
    error = TRUE
  )
})

test_that("unmatched() Errors when `by` columns in `table` aren't compatible with `comparison`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  bad_table <- test_df_a %>% mutate(vs = as.character(vs))
  expect_snapshot(slice_unmatched(bad_table, comp), error = TRUE)
})

test_that("slice_unmatched works when there are no unmatched", {
  df <- rownames_to_column(mtcars, "car")
  comp <- compare(df, df, by = "car")
  expect_identical(slice_unmatched(df, comp), as_tibble(df[0, ]))
})
