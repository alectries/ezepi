#' ezior: Odds ratio from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated incidence
#' odds ratio.
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
#' @return A tibble.
#' @importFrom fmsb oddsratio
#' @importFrom tibble tibble
#' @export
ezior <- function(x,
                 exposure_var,
                 outcome_var,
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
    c("xdat", "evar", "ovar", "iexp", "rexp", "iout", "rout"),
    as.list(match.call()[-1])
  )

  # generate a table with totals
  ezior.df <- ezepi:::table(
    x.df,
    index = TRUE,
    risk = FALSE,
    rate = FALSE
  )

  # calc odds ratio from table
  sink(file = nullfile())
  ezior.fmsb <- fmsb::oddsratio(
    ezior.df[ezior.df$exp == 'exposed', 'case'],
    ezior.df[ezior.df$exp == 'unexposed', 'case'],
    ezior.df[ezior.df$exp == 'exposed', 'control'],
    ezior.df[ezior.df$exp == 'unexposed', 'control'],
    conf.level = conf_lvl
  )
  sink()

  # pull numbers from fmsb for tibble
  ezior.res <- tibble::tibble(
    item = c(
      "Odds Ratio", "LCI", "UCI", "p-value"
    ),
    result = c(
      as.numeric(ezior.fmsb$estimate),
      as.numeric(ezior.fmsb$conf.int[1]),
      as.numeric(ezior.fmsb$conf.int[2]),
      as.numeric(ezior.fmsb$p.value)
    )
  )

  return(ezior.res)
}
