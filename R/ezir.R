#' ezir: Table with case count, person-time totals, and incidence rates from
#' binary exposure and outcome variables and a person-time variable.
#'
#' Returns a tibble with stratified counts, person-time totals, and rates by
#' exposure status.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble_row
#' @export
ezir <- function(x,
                  exposure_var,
                  outcome_var,
                  person_time,
                  index_exp = 1,
                  ref_exp = 0,
                  index_out = 1,
                  ref_out = 0
){
  # startup
  ## startup function will go here later

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "ptim", "iexp", "rexp", "iout", "rout"),
    modifyList(formals(ezepi::ezir), as.list(match.call()[-1]))
  )

  # generate a table with totals
  ezir.df <- ezepi:::table(
    x.df,
    index = TRUE,
    risk = NA,
    rate = TRUE
  )

  # add totals row
  ezir.res <- dplyr::bind_rows(
    ezir.df,
    tibble::tibble_row(
      !!names(ezir.df)[[1]] := "total",
      !!names(ezir.df)[[2]] := sum(ezir.df[,2], na.rm = T),
      !!names(ezir.df)[[3]] := sum(ezir.df[,3], na.rm = T),
      !!names(ezir.df)[[4]] := sum(ezir.df[,2], na.rm = T) / sum(ezir.df[,3], na.rm = T)
    )
  )

  # output
  return(ezir.res)
}
