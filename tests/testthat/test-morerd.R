# test that the function runs correctly
test_that("morerd generates correctly",{
  data(mtcars)
  rd <- riskdifference(8, 3, 11, 7) %>% unlist() %>% unname()
  test <- filter(mtcars, cyl != 8) %>%
            morerd(cyl, am, ref_exp = 6) %>%
            select(RD, LCI, UCI) %>%
            filter(exp == "exp.4")
  expect_equal(
    tibble(
      item = c("Risk Difference", "LCI", "UCI"),
      result = c(
        pull(test, RD),
        pull(test, LCI),
        pull(test, UCI)
      )
    ),
    tibble(
      item = c("Risk Difference", "LCI", "UCI"),
      result = c(
        rd %>% nth(4) %>% as.numeric(),
        rd %>% nth(2) %>% as.numeric(),
        rd %>% nth(3) %>% as.numeric()
      )
    )
  )
})

# test that the more function generates multiple rows
test_that("morerd generates multiple rows",{
  data(mtcars)
  expect_true(
    morerd(mtcars, cyl, am, ref_exp = 6) %>% nrow() == 3
  )
})

# test that the more function generates the correct columns
test_that("morerd generates correct columns",{
  data(mtcars)
  expect_identical(
    morerd(mtcars, cyl, am, ref_exp = 6) %>% names(),
    c("exp", "case", "control", "total", "risk", "RD", "LCI", "UCI")
  )
})

# test that the function runs incorrectly without data
test_that("morerd fails without data",{
  expect_error(
    morerd(nodata, cyl, am, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("morerd fails without exp",{
  data(mtcars)
  expect_error(
    morerd(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("morerd fails without out",{
  data(mtcars)
  expect_error(
    morerd(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("morerd fails with bad exp input class",{
  data(mtcars)
  expect_error(
    morerd(mtcars, cyl, am, ref_exp = "6"),
    regexp = "ezepi: Error: referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("morerd fails with bad exp input values",{
  data(mtcars)
  expect_error(
    morerd(mtcars, cyl, am, ref_exp = 5),
    regexp = "ezepi: Error: referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("morerd fails with bad out input class",{
  data(mtcars)
  expect_error(
    morerd(mtcars, cyl, am, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("morerd fails with bad out input values",{
  data(mtcars)
  expect_error(
    morerd(mtcars, cyl, am, ref_exp = 6, index_out = 3, ref_out = 5),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
