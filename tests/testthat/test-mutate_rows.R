# test that the function runs correctly
test_that("mutate_rows runs correctly",{
  data(mtcars)
  sample <- mtcars %>%
    as_tibble(rownames = "names")
  expect_true(
    mutate_rows(sample, total = rowSums(across(where(is.numeric))), .numeric_data = TRUE) %>%
      filter(names == "total") %>%
      pull(mpg) == 642.9
  )
})
