test_that("Error on input with duplicates", {
  without_dupe <- tibble(x = -5:2, y = -6:1, z = x)
  with_dupe <- slice(without_dupe, c(4, 4, 1))
  # for table_a
  expect_snapshot(
    compare(with_dupe, without_dupe, by = c(x, y)),
    error = TRUE
  )
  # for table_b
  expect_snapshot(
    compare(without_dupe, with_dupe, by = c(x, y)),
    error = TRUE
  )
  # many-to-many
  a <- mtcars[c(3, 3, 1), ]
  b <- mtcars[c(1, 3, 3), ]
  expect_snapshot(
    compare(a, b, by = all_of(names(mtcars))),
    error = TRUE
  )
})

test_that("Error when `by` columns are incompatible", {
  expect_snapshot(
    compare(test_df_a, test_df_b, by = c(car, wt, mpg)),
    error = TRUE
  )
})

test_that("Error on dupes when there are lots of `by` columns", {
  without_dupe <- setNames(seq_along(letters), letters) %>%
    as.list() %>%
    as_tibble()
  with_dupe <- without_dupe[c(1, 1, 2), ]
  expect_snapshot(
    compare(without_dupe, with_dupe, by = all_of(letters)),
    error = TRUE
  )
})

test_that("Error on dupes when there is a `by` column with a long name", {
  without_dupe <- setNames(seq_along(letters), letters) %>%
    as.list() %>%
    as_tibble() %>%
    frename(\(x) replace(x, 4, glue_collapse(letters, "z")))
  with_dupe <- without_dupe[c(1, 1, 2), ]
  expect_snapshot(
    compare(with_dupe, without_dupe, by = 1:6),
    error = TRUE
  )
})

test_that("Error on dupes when there is a `by` value with a large print width", {
  without_dupe <- tibble(a = glue_collapse(letters, "z"))
  with_dupe <- without_dupe[c(1, 1, 2), ]
  expect_snapshot(
    compare(with_dupe, without_dupe, by = a),
    error = TRUE
  )
})

test_that("Error on non data frame input", {
  non_df <- structure(list(), class = class(Sys.time()))
  expect_snapshot(
    compare(example_df_a, non_df, by = car),
    error = TRUE
  )
  expect_snapshot(
    compare(non_df, example_df_b, by = car),
    error = TRUE
  )
})

test_that("Error on input with duplicated names", {
  one <- data.frame(x = 1)
  two <- setNames(data.frame(x = 1, x = 1), c("x", "x"))

  expect_snapshot(compare(one, two, by = mpg), error = TRUE)
  # even when by is character (tidyselect doesn't handle this case)
  expect_snapshot(compare(one, two, by = "mpg"), error = TRUE)
})

test_that("Error on empty `by`", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = where(is.character)), error = TRUE)
})

test_that("Error when `by` columns don't match", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = where(is.numeric)), error = TRUE)
})

test_that("Error when columns in `by` aren't present", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = x), error = TRUE)
})

test_that("Error on named `by`", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = c(y = x)), error = TRUE)
})

test_that("Error when `by` uses `join_by`", {
  a <- data.frame(x = 1)
  b <- data.frame(x = 1)
  expect_snapshot(compare(a, b, by = join_by(x)), error = TRUE)
})

test_that("Error on different classes with coerce = FALSE", {
  expect_snapshot(
    compare(test_df_a, test_df_b, by = car, coerce = FALSE),
    error = TRUE
  )
  # but only if the classes are different
  df <- rownames_to_column(mtcars, "car")
  expect_identical(
    compare(df, df, by = car, coerce = FALSE),
    compare(df, df, by = car, coerce = TRUE)
  )
})

test_that("example comparison", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(comp)
  expect_snapshot(value_diffs_all(comp))
})

test_that("example comparison with allow_bothNA = FALSE", {
  comp <- compare(test_df_a, test_df_b, by = car, allow_both_NA = FALSE)
  expect_snapshot(comp)
  expect_snapshot(value_diffs_all(comp))
})

test_that("compare() works when table arguemnts aren't symbols", {
  comp <- compare(test_df_a %>% mutate(x = 1), test_df_b, by = car, allow_both_NA = FALSE)
  expect_equal(comp$tables$expr[1], "test_df_a %>% mutate(x = 1)")
})

test_that("compare() works when the tables only have one column", {
  a <- tibble(car = 1:4)
  b <- tibble(car = 2:5)
  expect_snapshot(compare(a, b, by = car))
})

test_that("compare() works when no rows are common", {
  a <- tibble(car = 1:2)
  b <- tibble(car = 5:6)
  expect_snapshot(compare(a, b, by = car))
})

test_that("compare() works when inputs are data tables", {
  dt_comp <- local({
    example_df_a <- data.table::as.data.table(example_df_a)
    example_df_b <- data.table::as.data.table(example_df_b)
    comp <- compare(example_df_a, example_df_b, by = car)
    attr(comp$unmatched_rows, ".internal.selfref") <- NULL
    comp
  })
  df_comp <- compare(example_df_a, example_df_b, by = car)

  expect_identical(dt_comp, df_comp)
})

test_that("summary() works", {
  comp <- compare(example_df_a, example_df_b, by = car)
  expect_identical(
    summary(comp),
    tibble(
      difference = c("value_diffs", "unmatched_cols", "unmatched_rows", "class_diffs"),
      found = c(TRUE, TRUE, TRUE, FALSE)
    )
  )
})
