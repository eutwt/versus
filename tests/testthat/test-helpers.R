test_that("shorten_works", {
  # return input unchanged
  expect_identical(
    shorten(c(a = "12345"), 5),
    c(a = "12345")
  )
  # length 1 input that is too long
  expect_identical(
    shorten(c(a = "123456"), 5),
    c(a = "12...")
  )
  # length > 1 input with some too-long elements
  expect_identical(
    shorten(c(a = "a", b = glue_collapse(1:10, ""), c = "c")),
    c(a = "a", b = "1234567...", c = "c")
  )
  # length > 1 input no too-long elements
  expect_identical(
    shorten(setNames(letters, letters)),
    setNames(letters, letters)
  )
  # numerics that aren't too long stay numeric (not needed but have test as FYI)
  expect_identical(
    shorten(c(1, 2, 3), 10),
    c(1, 2, 3)
  )
  # numerics that are too long are converted to character and shortened
  expect_identical(
    shorten(c(1, 123456789101112, 3), 10),
    c("1", "1234567...", "3")
  )
})

test_that("dottize works", {
  expect_identical(
    dottize(character(0), 8),
    character(0)
  )
  x <- "123456789"
  expect_identical(
    dottize(c(x, letters), 8),
    glue("12345...")
  )
  expect_identical(
    dottize(x, 100),
    glue(x)
  )
  expect_identical(
    dottize(c(x, letters), 10),
    glue("123456789, ...")
  )
  expect_identical(
    dottize(names(mtcars), 30),
    glue("mpg, cyl, disp, hp, drat, ...")
  )
})
