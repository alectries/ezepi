# test that the function runs correctly
test_that("moreior generates correctly",{
  data(mtcars)
  ior <- oddsratio(8, 3, 3, 4) %>% unlist() %>% unname()
  test <- filter(mtcars, cyl != 8) %>%
    moreior(cyl, am, ref_exp = 6) %>%
    select(IOR, LCI, UCI) %>%
    filter(exp == "exp.4")
  expect_equal(
    tibble(
      item = c("Odds Ratio", "LCI", "UCI"),
      result = c(
        pull(test, IOR),
        pull(test, LCI),
        pull(test, UCI)
      )
    ),
    tibble(
      item = c("Odds Ratio", "LCI", "UCI"),
      result = c(
        ior %>% nth(4) %>% as.numeric(),
        ior %>% nth(2) %>% as.numeric(),
        ior %>% nth(3) %>% as.numeric()
      )
    )
  )
})

# test that the more function generates multiple rows
test_that("moreior generates multiple rows",{
  data(mtcars)
  expect_true(
    moreior(mtcars, cyl, am, ref_exp = 6) %>% nrow() == 3
  )
})

# test that the more function generates the correct columns
test_that("moreior generates correct columns",{
  data(mtcars)
  expect_identical(
    moreior(mtcars, cyl, am, ref_exp = 6) %>% names(),
    c("exp", "case", "control", "total", "odds", "IOR", "LCI", "UCI")
  )
})

# test that the function runs incorrectly without data
test_that("moreior fails without data",{
  expect_error(
    moreior(nodata, cyl, am, index_exp = 4, ref_exp = 6)
  )
})

# test that the function runs incorrectly without exposure var
test_that("moreior fails without exp",{
  data(mtcars)
  expect_error(
    moreior(mtcars),
    regexp = "ezepi: Must specify exposure_var!"
  )
})

# test that the function runs incorrectly without outcome var
test_that("moreior fails without out",{
  data(mtcars)
  expect_error(
    moreior(mtcars, cyl),
    regexp = "ezepi: Must specify outcome_var!"
  )
})

# test that the function runs incorrectly with wrong class input for exp
test_that("moreior fails with bad exp input class",{
  data(mtcars)
  expect_error(
    moreior(mtcars, cyl, am, ref_exp = "6"),
    regexp = "ezepi: Error: referent exposure does not match variable type"
  )
})

# test that the function runs incorrectly with exp inputs missing in data
test_that("moreior fails with bad exp input values",{
  data(mtcars)
  expect_error(
    moreior(mtcars, cyl, am, ref_exp = 5),
    regexp = "ezepi: Error: referent value does not exist in exposure_var"
  )
})

# test that the function runs incorrectly with wrong class input for out
test_that("moreior fails with bad out input class",{
  data(mtcars)
  expect_error(
    moreior(mtcars, cyl, am, ref_exp = 6, index_out = "1", ref_out = "0"),
    regexp = "ezepi: Error: index/referent outcome does not match variable type"
  )
})

# test that the function runs incorrectly with out inputs missing in data
test_that("moreior fails with bad out input values",{
  data(mtcars)
  expect_error(
    moreior(mtcars, cyl, am, ref_exp = 6, index_out = 3, ref_out = 5),
    regexp = "ezepi: Error: index/referent value does not exist in outcome_var"
  )
})
