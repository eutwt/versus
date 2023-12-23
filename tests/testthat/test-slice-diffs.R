test_that("slice_diffs() works", {
  comp <- compare(example_df_a, example_df_b, by = car)
  expect_snapshot(slice_diffs(comp, "a", disp))
  expect_snapshot(slice_diffs(comp, "a", c(mpg, disp)))
  expect_identical(
    slice_diffs(comp, "a", c(wt, disp)),
    slice_diffs(comp, "a", disp)
  )
})

test_that("Error when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = car)
  expect_snapshot(
    example_df_a |> slice_diffs("a", disp),
    error = TRUE
  )
})

test_that("Error when `table` isn't 'a' or 'b'", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(slice_diffs(comp, a, disp), error = TRUE)
  expect_snapshot(slice_diffs(comp, disp), error = TRUE)
})

test_that("slice_diffs works when there are no diffs", {
  # because there are no diff cols
  df <- rownames_to_column(mtcars, "car")
  comp <- compare(df, df, by = "car")
  expect_identical(slice_diffs(comp, "a"), as_tibble(df[0, ]))
  # because there the supplied cols have no diffs
  comp <- compare(example_df_a, example_df_b, by = car)
  expect_identical(slice_diffs(comp, "a", wt), as_tibble(example_df_a[0, ]))
})
