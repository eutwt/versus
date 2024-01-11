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
})

test_that("allow_bothNA works", {
  comp <- compare(
    tibble(x = 1, y = NA),
    tibble(x = 1, y = NA),
    by = x,
    allow_both_NA = FALSE
  )
  expect_equal(1, filter(comp$intersection, column == "y")$n_diffs)

  comp <- compare(
    tibble(x = 1, y = 1),
    tibble(x = 1, y = NA),
    by = x,
    allow_both_NA = FALSE
  )
  expect_equal(1, filter(comp$intersection, column == "y")$n_diffs)

  comp <- compare(
    tibble(x = 1, y = NA),
    tibble(x = 1, y = NA),
    by = x,
    allow_both_NA = TRUE
  )
  expect_equal(0, filter(comp$intersection, column == "y")$n_diffs)

  comp <- compare(
    tibble(x = 1, y = 1),
    tibble(x = 1, y = NA),
    by = x,
    allow_both_NA = FALSE
  )
  expect_equal(1, filter(comp$intersection, column == "y")$n_diffs)
})

test_that("compare() works when table arguemnts aren't symbols", {
  comp <- compare(test_df_a %>% mutate(x = 1), test_df_b, by = car)
  expect_equal(comp$tables$expr[1], "test_df_a %>% mutate(x = 1)")
})

test_that("compare() works when no rows are common", {
  a <- tibble(car = 1:2, x = 1)
  b <- tibble(car = 5:6, x = 2)
  expect_snapshot(compare(a, b, by = car))
})

test_that("compare() works when no columns are common", {
  # tables have only one column
  a <- tibble(car = 1:4)
  b <- tibble(car = 2:5)
  expect_snapshot(compare(a, b, by = car))
  # tables have more than one column
  a <- tibble(car = 1:4, a = 1)
  b <- tibble(car = 2:5, b = 2)
  expect_snapshot(compare(a, b, by = car))
})

test_that("compare() works when no rows or columns are common", {
  # tables have only one column
  a <- tibble(car = 1:2)
  b <- tibble(car = 5:6)
  expect_snapshot(compare(a, b, by = car))
  # tables have more than one column
  a <- tibble(car = 1:2, a = 1)
  b <- tibble(car = 5:6, b = 2)
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

  expect_identical(
    dt_comp[setdiff(names(dt_comp), "input")],
    df_comp[setdiff(names(dt_comp), "input")]
  )
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

test_that("versus.copy_data_table option works", {
  dt <- data.table::data.table(x = 1)
  comp <- compare(dt, dt, by = x)
  expect_identical(comp$input$value$a, dt)
  comp <- with_options(compare(dt, dt, by = x), versus.copy_data_table = TRUE)
  expect_identical(comp$input$value$a, as_tibble(copy(dt)))
})

test_that("locate_matches() handles unmatched rows correctly", {
  # all common
  expect_snapshot(locate_matches(tibble(x = 1), tibble(x = 1), by = 'x'))
  # all different
  expect_snapshot(locate_matches(tibble(x = 1), tibble(x = 2), by = 'x'))
  # some different in each table
  expect_snapshot(locate_matches(tibble(x = 1:2), tibble(x = 2:3), by = 'x'))
  # some different in only one table
  expect_snapshot(locate_matches(tibble(x = 1:2), tibble(x = 2), by = 'x'))
  expect_snapshot(locate_matches(tibble(x = 2), tibble(x = 1:2), by = 'x'))
})
