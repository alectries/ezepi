#' ezrr: Risk ratio from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated risk ratio
#' (relative risk).
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @param print Whether to print a counts table to the console. Defaults to TRUE.
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom fmsb riskratio
#' @importFrom tibble tibble
#' @export
ezrr <- function(x,
                 exposure_var,
                 outcome_var,
                 index_exp = 1,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0,
                 conf_lvl = 0.95,
                 print = TRUE
){
  # startup
  ezepi:::startup(
    c("xdat", "evar", "ovar", "iexp", "rexp", "iout", "rout", "clvl"),
    utils::modifyList(formals(ezepi::ezrr), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "iexp", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::ezrr), as.list(match.call()[-1]))
  )

  # generate a table with totals
  ezrr.df <- ezepi:::table(
    x.df,
    index = TRUE,
    risk = TRUE,
    rate = FALSE,
    print = print
  )

  # calc risk ratio from table
  sink(file = nullfile())
  ezrr.fmsb <- fmsb::riskratio(
    ezrr.df[ezrr.df$exp == 'exposed', 'case'][[1]],
    ezrr.df[ezrr.df$exp == 'unexposed', 'case'][[1]],
    ezrr.df[ezrr.df$exp == 'exposed', 'total'][[1]],
    ezrr.df[ezrr.df$exp == 'unexposed', 'total'][[1]],
    conf.level = conf_lvl
  )
  sink()

  # pull numbers from fmsb for tibble
  ezrr.res <- tibble::tibble_row(
    "Risk Ratio" = as.numeric(ezrr.fmsb$estimate),
    "LCI" = as.numeric(ezrr.fmsb$conf.int[1]),
    "UCI" = as.numeric(ezrr.fmsb$conf.int[2]),
    "p-value" = as.numeric(ezrr.fmsb$p.value)
  )

  return(ezrr.res)
}
