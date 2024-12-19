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
#' @importFrom magrittr `%>%`
#' @importFrom rlang `:=`
#' @importFrom rlang `!!`
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @export

mutate_rows <- function(
    x,
    ...,
    .numeric_data = FALSE
  ){
  # setup
  `%>%` <- magrittr::`%>%`
  `:=` <- rlang::`:=`
  `!!` <- rlang::`!!`
  ezepi:::startup(
    c("xdat", "numd"),
    utils::modifyList(formals(ezepi::mutate_rows), as.list(match.call()[-1]))
  )

  # get first column name
  orig_name <- names(x)[1]

  # add rows (data is numeric)
  if(.numeric_data){
    x.1 <- x %>% ezepi::ezt(row_name = "temp_headers", numeric_data = TRUE)
    x.m <- dplyr::mutate(x.1, ...)
    x.0 <- x.m %>% ezepi::ezt(row_name = "temp", numeric_data = TRUE)
    x.f <- dplyr::rename(x.0, !!paste0(orig_name) := temp)
  }

  # add rows (data is non-numeric)
  if(!.numeric_data){
    x.1 <- x %>% ezepi::ezt(row_name = "temp_headers", numeric_data = FALSE)
    x.m <- dplyr::mutate(x.1, ...)
    x.0 <- x.m %>% ezepi::ezt(row_name = "temp", numeric_data = FALSE)
    x.f <- dplyr::rename(x.0, !!paste0(orig_name) := temp)
  }

  return(x.f)
}
