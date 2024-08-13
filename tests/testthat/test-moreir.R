# test that the function runs correctly
test_that("moreir generates correctly",{
  data(mtcars)
  expect_equal(
    moreir(mtcars, cyl, am, carb, ref_exp = 8),
    tibble(
      exp = c("exp.4", "exp.6", "unexposed", "sum"),
      case = c(8, 3, 2, 13),
      pt = c(17, 24, 49, 90),
      rate = c(8/17, 3/24, 2/49, 13/90)
    )
  )
})

# test that the function runs incorrectly without data
test_that("moreir fails without data",{
  expect_error(
    moreir(nodata, cyl, am, carb, ref_exp = 8)
  )
})

# test that the function runs incorrectly without exposure var
test_that("moreir fails without exp",{
  data(mtcars)
  expect_error(
    moreir(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("moreir fails without out",{
  data(mtcars)
  expect_error(
    moreir(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly without person-time var
test_that("moreir fails without pt",{
  data(mtcars)
  expect_error(
    moreir(mtcars, cyl, am),
    regexp = "ezepi: Must specify person_time!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("moreir fails with bad exp input class",{
  data(mtcars)
  expect_error(
    moreir(mtcars, cyl, am, carb, ref_exp = "6"),
    regexp = "ezepi: Error: referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("moreir fails with bad exp input values",{
  data(mtcars)
  expect_error(
    moreir(mtcars, cyl, am, carb, ref_exp = 5),
    regexp = "ezepi: Error: referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("moreir fails with bad out input class",{
  data(mtcars)
  expect_error(
    moreir(mtcars, cyl, am, carb, ref_exp = 8, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("moreir fails with bad out input values",{
  data(mtcars)
  expect_error(
    moreir(mtcars, cyl, am, carb, ref_exp = 8, index_out = 4, ref_out = 6),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})

# test that the function runs incorrectly if person-time is not numeric
test_that("moreir fails with bad pt class",{
  data(mtcars)
  expect_error(
    mtcars %>%
      mutate(carb = as.character(carb)) %>%
      moreir(cyl, am, carb, ref_exp = 8)
  )
})
