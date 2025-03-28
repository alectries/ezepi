#' moreird: Rate difference from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts, rates, and rate differences with associated CIs.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @param print Whether to print a counts table to the console. Defaults to TRUE.
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom fmsb ratedifference
#' @importFrom tibble tibble_row
#' @importFrom dplyr bind_rows
#' @export
moreird <- function(x,
                    exposure_var,
                    outcome_var,
                    person_time,
                    ref_exp = 0,
                    index_out = 1,
                    ref_out = 0,
                    conf_lvl = 0.95,
                    print = TRUE
){
  # startup
  ezepi:::startup(
    c("xdat", "evar", "ovar", "ptim", "rexp", "iout", "rout", "clvl"),
    utils::modifyList(formals(ezepi::moreird), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "ptim", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::moreird), as.list(match.call()[-1]))
  )

  # generate a table with totals
  moreird.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = NA,
    rate = TRUE,
    print = print
  )

  # calc rate diffs and CIs to add to table
  moreird.list <- list()
  for(i in 1:nrow(moreird.df)){
    # calc rate diff
    sink(file = nullfile())
    moreird.fmsb <- fmsb::ratedifference(
      moreird.df[i, 'case'][[1]],
      moreird.df[moreird.df$exp == 'unexposed', 'case'][[1]],
      moreird.df[i, 'pt'][[1]],
      moreird.df[moreird.df$exp == 'unexposed', 'pt'][[1]],
      CRC = TRUE,
      conf.level = conf_lvl
    )
    sink()

    # add row to list
    moreird.list[[i]] <- tibble::tibble_row(
      "Exposure" = moreird.df$exp[[i]],
      "Rate Difference" = as.numeric(moreird.fmsb$estimate),
      "LCI" = as.numeric(moreird.fmsb$conf.int[1]),
      "UCI" = as.numeric(moreird.fmsb$conf.int[2]),
      "p-value" = as.numeric(moreird.fmsb$p.value)
    )
  }

  # combine into table
  moreird.res <- dplyr::bind_rows(moreird.list)

  return(moreird.res)
}
