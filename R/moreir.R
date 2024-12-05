#' moreir: nx2 table with case counts, person-time totals, and incidence rates from
#' categorical exposure and outcome variables and a person-time variable.
#'
#' Returns a tibble with stratified counts, person-time totals, and rates by
#' exposure status.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble_row
#' @export
moreir <- function(x,
                 exposure_var,
                 outcome_var,
                 person_time,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0
){
  # startup
  ## startup function will go here later

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "ptim", "rexp", "iout", "rout"),
    modifyList(formals(ezepi::moreir), as.list(match.call()[-1]))
  )

  # generate a table with totals
  moreir.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = NA,
    rate = TRUE
  )

  # add totals row
  moreir.res <- dplyr::bind_rows(
    moreir.df,
    tibble::tibble_row(
      !!names(moreir.df)[[1]] := "total",
      !!names(moreir.df)[[2]] := sum(moreir.df[,2], na.rm = T),
      !!names(moreir.df)[[3]] := sum(moreir.df[,3], na.rm = T),
      !!names(moreir.df)[[4]] := sum(moreir.df[,2], na.rm = T) / sum(moreir.df[,3], na.rm = T)
    )
  )

  # output
  return(moreir.res)
}
