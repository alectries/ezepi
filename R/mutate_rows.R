#' mutate_rows: Create, modify, and delete rows
#'
#' Like \code{mutate()} in \code{dplyr}, create new rows that are functions of existing rows. You can modify and delete rows by setting their value to null.
#'
#' This function transposes the dataframe (using \code{ezt()}), passes its arguments to \code{mutate()}, then transposes it again. This means it works exactly as you would expect \code{mutate()} to work.
#'
#' The first column of your dataset must be a unique identifier. Add one if necessary using \code{rowid_to_column()}.
#'
#' @param x A dataset.
#' @param ... Name-value pairs and other arguments. See \code{\link[dplyr]{mutate}}.
#' @return A tibble.
#' @export

mutate_rows <- function(x, ...){
  # setup
  ## check that required packages are loaded
  if("dplyr" %in% (.packages())){} else {
    stop("ezepi: ezepi requires the tidyverse. Please execute library(tidyverse) or library(ezepi) before continuing.")
  }
  if("fmsb" %in% (.packages())){} else {
    stop("ezepi: ezepi requires fmsb. Please execute library(fmsb) or library(ezepi) before continuing.")
  }
  ## check that required vars exist
  if(
    is_empty({{x}})
  ){
    stop("ezepi: Must specify a dataset!")
  }

  # get first column name
  orig_name <- names(x)[1]

  # add rows
  x %>%
    ezt(row_name = "temp_headers") %>%
    mutate(...) %>%
    ezt(row_name = orig_name) %>%
    return()
}
