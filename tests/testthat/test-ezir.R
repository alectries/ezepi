# test that the function runs correctly
test_that("ezir generates correctly",{
  data(mtcars)
  expect_equal(
    ezir(mtcars, cyl, am, carb, index_exp = 4, ref_exp = 6),
    tibble(
      exp = c("exposed", "unexposed", "sum"),
      case = c(8, 3, 11),
      pt = c(17, 24, 41),
      rate = c(8/17, 3/24, 11/41)
    )
  )
})

# test that the function runs incorrectly without data
test_that("ezir fails without data",{
  expect_error(
    ezir(nodata, cyl, am, carb, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("ezir fails without exp",{
  data(mtcars)
  expect_error(
    ezir(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("ezir fails without out",{
  data(mtcars)
  expect_error(
    ezir(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly without person-time var
test_that("ezir fails without pt",{
  data(mtcars)
  expect_error(
    ezir(mtcars, cyl, am),
    regexp = "ezepi: Must specify person_time!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("ezir fails with bad exp input class",{
  data(mtcars)
  expect_error(
    ezir(mtcars, cyl, am, carb, index_exp = "4", ref_exp = "6"),
    regexp = "ezepi: Error: index/referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("ezir fails with bad exp input values",{
  data(mtcars)
  expect_error(
    ezir(mtcars, cyl, am, carb, index_exp = 3, ref_exp = 5),
    regexp = "ezepi: Error: index/referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("ezir fails with bad out input class",{
  data(mtcars)
  expect_error(
    ezir(mtcars, cyl, am, carb, index_exp = 4, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("ezir fails with bad out input values",{
  data(mtcars)
  expect_error(
    ezir(mtcars, cyl, am, carb, index_exp = 4, ref_exp = 6, index_out = 4, ref_out = 6),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})

# test that the function runs incorrectly if person-time is not numeric
test_that("ezir fails with bad pt class",{
  data(mtcars)
  expect_error(
    mtcars %>%
      mutate(carb = as.character(carb)) %>%
      ezir(cyl, am, carb, index_exp = 4, ref_exp = 6)
  )
})
