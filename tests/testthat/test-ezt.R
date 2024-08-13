# test that the function runs correctly
test_that("ezt flips table correctly with rowname and num",{
  data(mtcars)
  sample <- mtcars %>%
    slice(1:4) %>%
    select(cyl, am) %>%
    as_tibble(rownames = NULL) %>%
    add_column(names = c("one", "two", "three", "four"),
               .before = 1)
  expect_equal(
    ezt(sample, row_name = "vals", numeric_data = TRUE),
    tibble(
      "vals" = c("cyl", "am"),
      "one" = as.numeric(c(6, 1)),
      "two" = as.numeric(c(6, 1)),
      "three" = as.numeric(c(4, 1)),
      "four" = as.numeric(c(6, 0))
    )
  )
})

# test that the function runs correctly without row_name
test_that("ezt flips table without row_name",{
  data(mtcars)
  sample <- mtcars %>%
    slice(1:4) %>%
    select(cyl, am) %>%
    as_tibble(rownames = NULL) %>%
    add_column(names = c("one", "two", "three", "four"),
               .before = 1)
  expect_equal(
    ezt(sample, numeric_data = TRUE),
    tibble(
      "one" = as.numeric(c(6, 1)),
      "two" = as.numeric(c(6, 1)),
      "three" = as.numeric(c(4, 1)),
      "four" = as.numeric(c(6, 0))
    )
  )
})

# test that the function runs correctly without numeric data on
test_that("ezt flips table with num off",{
  data(mtcars)
  sample <- mtcars %>%
    slice(1:4) %>%
    select(cyl, am) %>%
    as_tibble(rownames = NULL) %>%
    add_column(names = c("one", "two", "three", "four"),
               .before = 1)
  expect_equal(
    ezt(sample, row_name = "vals"),
    tibble(
      "vals" = c("cyl", "am"),
      "one" = as.character(c(6, 1)),
      "two" = as.character(c(6, 1)),
      "three" = as.character(c(4, 1)),
      "four" = as.character(c(6, 0))
    )
  )
})

# test that the function runs correctly with numeric data off and without row_name
test_that("ezt flips table with num off and no row_name",{
  data(mtcars)
  sample <- mtcars %>%
    slice(1:4) %>%
    select(cyl, am) %>%
    as_tibble(rownames = NULL) %>%
    add_column(names = c("one", "two", "three", "four"),
               .before = 1)
  expect_equal(
    ezt(sample),
    tibble(
      "one" = as.character(c(6, 1)),
      "two" = as.character(c(6, 1)),
      "three" = as.character(c(4, 1)),
      "four" = as.character(c(6, 0))
    )
  )
})

# test that the function runs incorrectly without data
test_that("ezt fails without data",{
  expect_error(
    ezt(nodata, row_name = "vals", numeric_data = TRUE),
  )
})

# test that the function runs incorrectly when row_name is not a string
test_that("ezt fails with bad row_name class",{
  sample <- mtcars %>%
    slice(1:4) %>%
    select(cyl, am) %>%
    as_tibble(rownames = NULL) %>%
    add_column(names = c("one", "two", "three", "four"),
               .before = 1)
  expect_error(
    ezt(sample, row_name = 5, numeric_data = TRUE),
    regexp = "ezepi: If not null, row_name must be a string."
  )
})

# test that the function gets angry when character is passed as numeric
test_that("ezt fails with bad data class",{
  sample <- tibble(
    "one" = c("A", "B"),
    "two" = c("C", "D")
  )
  expect_warning(
    ezt(sample, row_name = "number", numeric_data = TRUE)
  )
})
