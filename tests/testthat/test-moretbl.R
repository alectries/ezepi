# test that the function runs correctly
test_that("moretbl generates correctly",{
  data(mtcars)
  expect_equal(
    moretbl(mtcars, cyl, am, ref_exp = 8),
    tibble(
      exp = c("exp.4", "exp.6", "unexposed", "sum"),
      case = c(8, 3, 2, 13),
      control = c(3, 4, 12, 19),
      total = c(11, 7, 14, 32),
      risk = c(8/11, 3/7, 2/14, 13/32)
    )
  )
})

# test that the function runs incorrectly without data
test_that("moretbl fails without data",{
  expect_error(
    moretbl(nodata, cyl, am, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("moretbl fails without exp",{
  data(mtcars)
  expect_error(
    moretbl(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("moretbl fails without out",{
  data(mtcars)
  expect_error(
    moretbl(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("moretbl fails with bad exp input class",{
  data(mtcars)
  expect_error(
    moretbl(mtcars, cyl, am, ref_exp = "6"),
    regexp = "ezepi: Error: referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("moretbl fails with bad exp input values",{
  data(mtcars)
  expect_error(
    moretbl(mtcars, cyl, am, ref_exp = 5),
    regexp = "ezepi: Error: referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("moretbl fails with bad out input class",{
  data(mtcars)
  expect_error(
    moretbl(mtcars, cyl, am, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("moretbl fails with bad out input values",{
  data(mtcars)
  expect_error(
    moretbl(mtcars, cyl, am, ref_exp = 6, index_out = 4, ref_out = 6),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
