#' moretbl: nx2 contingency table from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts by exposure and outcome, along with
#' sums and risks or odds by exposure status. Like eztbl, but allows for more
#' exposure categories (but still binary outcome).
#'
#' Note: Before using moretbl, you must collapse all missing variables to NA.
#' All non-null values of exposure_var will be included in the analysis.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param risk If TRUE, calculates risks. If FALSE, calculates odds.
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom rlang `:=`
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble_row
#' @export
moretbl <- function(x,
                  exposure_var,
                  outcome_var,
                  ref_exp = 0,
                  index_out = 1,
                  ref_out = 0,
                  risk = TRUE
){
  # startup
  `:=` <- rlang::`:=`
  ezepi:::startup(
    c("xdat", "evar", "ovar", "rexp", "iout", "rout", "risk"),
    utils::modifyList(formals(ezepi::moretbl), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::moretbl), as.list(match.call()[-1]))
  )

  # generate a table with totals
  eztbl.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = risk,
    rate = FALSE
  )

  # add totals row
  eztbl.res <- dplyr::bind_rows(
    eztbl.df,
    tibble::tibble_row(
      !!names(eztbl.df)[[1]] := "total",
      !!names(eztbl.df)[[2]] := sum(eztbl.df[,2], na.rm = T),
      !!names(eztbl.df)[[3]] := sum(eztbl.df[,3], na.rm = T),
      !!names(eztbl.df)[[4]] := sum(eztbl.df[,4], na.rm = T),
      !!names(eztbl.df)[[5]] := ifelse(
        risk,
        sum(eztbl.df[,2], na.rm = T) / sum(eztbl.df[,4], na.rm = T),
        sum(eztbl.df[,2], na.rm = T) / sum(eztbl.df[,3], na.rm = T)
      )
    )
  )

  # output
  return(eztbl.res)
}
