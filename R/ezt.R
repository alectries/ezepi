#' ezt: Transpose a dataset
#'
#' Returns a tibble with horizontal and vertical axes reversed from the original dataset.
#'
#' In some cases, the data you want to be your headers may be included as row names rather than a variable. If this is the case, pipe \code{as_tibble(x, rownames = "names")} into \code{ezt()}.
#'
#' @param x A dataset.
#' @param row_name A string, which will be the name of the left-most column. Defaults to NULL, which will exclude the existing dataset's headers from the new dataset.
#' @param numeric_data Defaults to FALSE. When TRUE, data except the names column will be coerced to numeric.
#' @return A tibble.
#' @importFrom rlang `:=`
#' @importFrom tibble tibble
#' @importFrom tibble add_column
#' @importFrom dplyr slice
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @export

ezt <- function(
    x,
    row_name = NULL,
    numeric_data = FALSE
){
  # startup
  `:=` <- rlang::`:=`
  ## startup function will go here later

  # loop through columns
  x.df <- tibble::tibble(.rows = length(names(x)))
  for(i in 1:nrow(x)){
    x.df <- tibble::add_column(
      x.df,
      !!paste0(i) := as.vector(unlist(dplyr::slice(x, i)))
    )
  }

  # set column names
  x.df <- dplyr::slice(
    setNames(x.df, dplyr::slice(x.df, 1)),
    -1
  )

  # coerce to numeric if needed
  if(numeric_data){
    x.df <- dplyr::mutate(
      x.df,
      dplyr::across(dplyr::everything(), as.numeric)
    )
  }

  # if row_name is specified, create names column
  if(!is.null(row_name)){
    x.df <- tibble::add_column(
      x.df,
      !!row_name := names(x)[-1],
      .before = 1
    )
  }

  return(x.df)
}
