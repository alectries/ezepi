#' mhrr: Mantel-Haenszel pooled risk ratio from binary exposure and outcome variables
#'
#' Returns a tibble with a 95 percent CI and estimated risk ratio
#' (relative risk).
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param strat_var A categorical stratification variable in x to pool across.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom fmsb RRMH
#' @importFrom tibble tibble
#' @export
mhrr <- function(x,
                 exposure_var,
                 outcome_var,
                 strat_var,
                 index_exp = 1,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0,
                 conf_lvl = 0.95
){
  # startup
  ezepi:::startup(
    c("xdat", "evar", "ovar", "svar", "iexp", "rexp", "iout", "rout", "clvl"),
    utils::modifyList(formals(ezepi::mhrr), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "svar", "iexp", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::mhrr), as.list(match.call()[-1]))
  )

  # generate a matrix for fmsb
  mhrr.df <- ezepi:::matrix(
    x.df,
    index = TRUE,
    risk = TRUE,
    rate = FALSE
  )

  # calc risk ratio from table
  sink(file = nullfile())
  mhrr.fmsb <- fmsb::RRMH(
    mhrr.df,
    conf.level = conf_lvl
  )
  sink()

  # pull numbers from fmsb for tibble
  mhrr.res <- tibble::tibble(
    "Risk Ratio" = as.numeric(mhrr.fmsb$estimate),
    "LCI" = as.numeric(mhrr.fmsb$conf.int[1]),
    "UCI" = as.numeric(mhrr.fmsb$conf.int[2])
  )

  return(mhrr.res)
}
