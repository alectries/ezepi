# test that the function runs correctly
test_that("moreprev generates correctly",{
  data(mtcars)
  expect_equal(
    moreprev(mtcars, am),
    tibble(
      out = c("0", "1", "total"),
      count = c(19, 13, 32),
      prevalence = c(19/32, 13/32, 32/32)
    )
  )
})

# test that the function runs incorrectly without data
test_that("moreprev fails without data",{
  expect_error(
    moreprev(nodata, am)
  )
})

# test that the function runs incorrectly without outcome var
test_that("moreprev fails without out",{
  data(mtcars)
  expect_error(
    moreprev(mtcars),
    regexp = "ezepi: Must specify outcome_var!"
  )
})
