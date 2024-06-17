# test that the function runs correctly
test_that("prev generates correctly",{
  data(mtcars)
  expect_equal(
    ezprev(mtcars, am),
    tibble(
      case = c(13),
      control = c(19),
      total = c(32),
      prevalence = c(13/32)
    )
  )
})

# test that the function runs incorrectly without data
test_that("prev fails without data",{
  expect_error(
    ezprev(nodata, am)
  )
})

# test that the function runs incorrectly without outcome var
test_that("prev fails without out",{
  data(mtcars)
  expect_error(
    ezprev(mtcars),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("prev fails with bad out input class",{
  data(mtcars)
  expect_error(
    ezprev(mtcars, am, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("prev fails with bad out input values",{
  data(mtcars)
  expect_error(
    ezprev(mtcars, am, index_out = 4, ref_out = 6),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
