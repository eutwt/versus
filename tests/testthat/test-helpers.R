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
