# test that the function runs correctly
test_that("tbl generates correctly",{
  data(mtcars)
  expect_equal(
    eztbl(mtcars, cyl, am, index_exp = 4, ref_exp = 6),
    tibble(
      exp = c("exposed", "unexposed", "sum"),
      case = c(8, 3, 11),
      control = c(3, 4, 7),
      total = c(11, 7, 18),
      risk = c(8/11, 3/7, 11/18)
    )
  )
})

# test that the function runs incorrectly without data
test_that("tbl fails without data",{
  expect_error(
    eztbl(nodata, cyl, am, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("tbl fails without exp",{
  data(mtcars)
  expect_error(
    eztbl(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("tbl fails without out",{
  data(mtcars)
  expect_error(
    eztbl(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("tbl fails with bad exp input class",{
  data(mtcars)
  expect_error(
    eztbl(mtcars, cyl, am, index_exp = "4", ref_exp = "6"),
    regexp = "ezepi: Error: index/referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("tbl fails with bad exp input values",{
  data(mtcars)
  expect_error(
    eztbl(mtcars, cyl, am, index_exp = 3, ref_exp = 5),
    regexp = "ezepi: Error: index/referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("tbl fails with bad out input class",{
  data(mtcars)
  expect_error(
    eztbl(mtcars, cyl, am, index_exp = 4, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("tbl fails with bad out input values",{
  data(mtcars)
  expect_error(
    eztbl(mtcars, cyl, am, index_exp = 4, ref_exp = 6, index_out = 4, ref_out = 6),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
