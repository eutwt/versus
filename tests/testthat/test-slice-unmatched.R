test_that("slice_unmatched_both works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(slice_unmatched_both(comp))
})

test_that("slice_unmatched works", {
  comp <- compare(example_df_a, example_df_b, by = car)
  expect_identical(slice_unmatched(comp, "a"), as_tibble(example_df_a[7, ]))
})

test_that("unmatched() errors when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = car)
  expect_snapshot(
    example_df_a |> slice_unmatched(disp),
    error = TRUE
  )
})

test_that("slice_unmatched works when there are no unmatched", {
  df <- rownames_to_column(mtcars, "car")
  comp <- compare(df, df, by = "car")
  expect_identical(slice_unmatched(comp, "a"), as_tibble(df[0, ]))
})
