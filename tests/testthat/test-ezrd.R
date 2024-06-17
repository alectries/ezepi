# test that the function runs correctly
test_that("ezrd generates correctly",{
  data(mtcars)
  rd <- riskdifference(8, 3, 11, 7) %>% unlist() %>% unname()
  expect_equal(
    ezrd(mtcars, cyl, am, index_exp = 4, ref_exp = 6),
    tibble(
      item = c("Risk Difference", "LCI", "UCI", "p-value"),
      result = c(
        rd %>% nth(4) %>% as.numeric(),
        rd %>% nth(2) %>% as.numeric(),
        rd %>% nth(3) %>% as.numeric(),
        rd %>% nth(1) %>% as.numeric()
      )
    )
  )
})

# test that the function runs incorrectly without data
test_that("ezrd fails without data",{
  expect_error(
    ezrd(nodata, cyl, am, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("ezrd fails without exp",{
  data(mtcars)
  expect_error(
    ezrd(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("ezrd fails without out",{
  data(mtcars)
  expect_error(
    ezrd(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("ezrd fails with bad exp input class",{
  data(mtcars)
  expect_error(
    ezrd(mtcars, cyl, am, index_exp = "4", ref_exp = "6"),
    regexp = "ezepi: Error: index/referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("ezrd fails with bad exp input values",{
  data(mtcars)
  expect_error(
    ezrd(mtcars, cyl, am, index_exp = 3, ref_exp = 5),
    regexp = "ezepi: Error: index/referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("ezrd fails with bad out input class",{
  data(mtcars)
  expect_error(
    ezrd(mtcars, cyl, am, index_exp = 4, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("ezrd fails with bad out input values",{
  data(mtcars)
  expect_error(
    ezrd(mtcars, cyl, am, index_exp = 4, ref_exp = 6, index_out = 3, ref_out = 5),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
