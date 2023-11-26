test_that("slice_diffs_both works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(as_tibble(out))
})

test_that("slice_diffs_both works with multi-variable `by`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(as_tibble(out))
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
