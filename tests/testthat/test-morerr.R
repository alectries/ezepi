# test that the function runs correctly
test_that("morerr generates correctly",{
  data(mtcars)
  rd <- riskratio(8, 3, 11, 7) %>% unlist() %>% unname()
  test <- filter(mtcars, cyl != 8) %>%
    morerr(cyl, am, ref_exp = 6) %>%
    select(RR, LCI, UCI) %>%
    filter(exp == "exp.4")
  expect_equal(
    tibble(
      item = c("Risk Ratio", "LCI", "UCI"),
      result = c(
        pull(test, RR),
        pull(test, LCI),
        pull(test, UCI)
      )
    ),
    tibble(
      item = c("Risk Ratio", "LCI", "UCI"),
      result = c(
        rd %>% nth(4) %>% as.numeric(),
        rd %>% nth(2) %>% as.numeric(),
        rd %>% nth(3) %>% as.numeric()
      )
    )
  )
})

# test that the more function generates multiple rows
test_that("morerr generates multiple rows",{
  data(mtcars)
  expect_true(
    morerr(mtcars, cyl, am, ref_exp = 6) %>% nrow() == 3
  )
})

# test that the more function generates the correct columns
test_that("morerr generates correct columns",{
  data(mtcars)
  expect_identical(
    morerr(mtcars, cyl, am, ref_exp = 6) %>% names(),
    c("exp", "case", "control", "total", "risk", "RR", "LCI", "UCI")
  )
})

# test that the function runs incorrectly without data
test_that("morerr fails without data",{
  expect_error(
    morerr(nodata, cyl, am, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("morerr fails without exp",{
  data(mtcars)
  expect_error(
    morerr(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("morerr fails without out",{
  data(mtcars)
  expect_error(
    morerr(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("morerr fails with bad exp input class",{
  data(mtcars)
  expect_error(
    morerr(mtcars, cyl, am, ref_exp = "6"),
    regexp = "ezepi: Error: referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("morerr fails with bad exp input values",{
  data(mtcars)
  expect_error(
    morerr(mtcars, cyl, am, ref_exp = 5),
    regexp = "ezepi: Error: referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("morerr fails with bad out input class",{
  data(mtcars)
  expect_error(
    morerr(mtcars, cyl, am, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("morerr fails with bad out input values",{
  data(mtcars)
  expect_error(
    morerr(mtcars, cyl, am, ref_exp = 6, index_out = 3, ref_out = 5),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
