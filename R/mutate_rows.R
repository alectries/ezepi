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
#' @param .numeric_data Defaults to FALSE. When TRUE, data except the names column will be coerced to numeric.
#' @return A tibble.
#' @importFrom dplyr mutate
#' @export

mutate_rows <- function(
    x,
    ...,
    .numeric_data = FALSE
  ){
  # setup
  ## startup function will go here later

  # get first column name
  orig_name <- names(x)[1]

  # add rows
  x.1 <- ezepi::ezt(x, row_name = "temp_headers", numeric_data = .numeric_data)
  x.m <- dplyr::mutate(x.1, ...)
  x.0 <- ezepi::ezt(x.m, row_name = orig_name, numeric_data = .numeric_data)

  return(x.0)
}
