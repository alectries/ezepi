#' ezirr: Rate ratio from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated incidence
#' rate ratio.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @importFrom fmsb rateratio
#' @importFrom tibble tibble
#' @export
ezirr <- function(x,
                 exposure_var,
                 outcome_var,
                 person_time,
                 index_exp = 1,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0,
                 conf_lvl = 0.95
){
  # startup
  ## startup function will go here later

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "ptim", "iexp", "rexp", "iout", "rout"),
    modifyList(formals(ezepi::ezirr), as.list(match.call()[-1]))
  )

  # generate a table with totals
  ezirr.df <- ezepi:::table(
    x.df,
    index = TRUE,
    risk = NA,
    rate = TRUE
  )
  print(ezirr.df)

  # calc rate ratio from table
  sink(file = nullfile())
  ezirr.fmsb <- fmsb::rateratio(
    ezirr.df[ezirr.df$exp == 'exposed', 'case'][[1]],
    ezirr.df[ezirr.df$exp == 'unexposed', 'case'][[1]],
    ezirr.df[ezirr.df$exp == 'exposed', 'pt'][[1]],
    ezirr.df[ezirr.df$exp == 'unexposed', 'pt'][[1]],
    conf.level = conf_lvl
  )
  sink()

  # pull numbers from fmsb for tibble
  ezirr.res <- tibble::tibble(
    "Rate Ratio" = as.numeric(ezirr.fmsb$estimate),
    "LCI" = as.numeric(ezirr.fmsb$conf.int[1]),
    "UCI" = as.numeric(ezirr.fmsb$conf.int[2]),
    "p-value" = as.numeric(ezirr.fmsb$p.value)
  )

  return(ezirr.res)
}
