# test that the function runs correctly
test_that("ezrr generates correctly",{
  data(mtcars)
  rr <- riskratio(8, 3, 11, 7) %>% unlist() %>% unname()
  expect_equal(
    ezrr(mtcars, cyl, am, index_exp = 4, ref_exp = 6),
    tibble(
      item = c("Risk Ratio", "LCI", "UCI", "p-value"),
      result = c(
        rr %>% nth(4) %>% as.numeric(),
        rr %>% nth(2) %>% as.numeric(),
        rr %>% nth(3) %>% as.numeric(),
        rr %>% nth(1) %>% as.numeric()
      )
    )
  )
})

# test that the function runs incorrectly without data
test_that("ezrr fails without data",{
  expect_error(
    ezrr(nodata, cyl, am, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("ezrr fails without exp",{
  data(mtcars)
  expect_error(
    ezrr(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("ezrr fails without out",{
  data(mtcars)
  expect_error(
    ezrr(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("ezrr fails with bad exp input class",{
  data(mtcars)
  expect_error(
    ezrr(mtcars, cyl, am, index_exp = "4", ref_exp = "6"),
    regexp = "ezepi: Error: index/referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("ezrr fails with bad exp input values",{
  data(mtcars)
  expect_error(
    ezrr(mtcars, cyl, am, index_exp = 3, ref_exp = 5),
    regexp = "ezepi: Error: index/referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("ezrr fails with bad out input class",{
  data(mtcars)
  expect_error(
    ezrr(mtcars, cyl, am, index_exp = 4, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("ezrr fails with bad out input values",{
  data(mtcars)
  expect_error(
    ezrr(mtcars, cyl, am, index_exp = 4, ref_exp = 6, index_out = 3, ref_out = 5),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
