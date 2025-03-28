#' morerr: Multiple risk ratios from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts, risks, and ratios with associated CIs.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @param print Whether to print a counts table to the console. Defaults to TRUE.
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom fmsb riskratio
#' @importFrom tibble tibble_row
#' @importFrom dplyr bind_rows
#' @export
morerr <- function(x,
                   exposure_var,
                   outcome_var,
                   ref_exp = 0,
                   index_out = 1,
                   ref_out = 0,
                   conf_lvl = 0.95,
                   print = TRUE
){
  # startup
  ezepi:::startup(
    c("xdat", "evar", "ovar", "rexp", "iout", "rout", "clvl"),
    utils::modifyList(formals(ezepi::morerr), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::morerr), as.list(match.call()[-1]))
  )

  # generate a table with totals
  morerr.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = TRUE,
    rate = FALSE,
    print = print
  )

  # calc risk ratios and CIs to add to table
  morerr.list <- list()
  for(i in 1:nrow(morerr.df)){
    # calc risk ratio
    sink(file = nullfile())
    morerr.fmsb <- fmsb::riskratio(
      morerr.df[i, 'case'][[1]],
      morerr.df[morerr.df$exp == 'unexposed', 'case'][[1]],
      morerr.df[i, 'total'][[1]],
      morerr.df[morerr.df$exp == 'unexposed', 'total'][[1]],
      conf.level = conf_lvl
    )
    sink()

    # add row to list
    morerr.list[[i]] <- tibble::tibble_row(
      "Exposure" = morerr.df$exp[[i]],
      "Risk Ratio" = as.numeric(morerr.fmsb$estimate),
      "LCI" = as.numeric(morerr.fmsb$conf.int[1]),
      "UCI" = as.numeric(morerr.fmsb$conf.int[2]),
      "p-value" = as.numeric(morerr.fmsb$p.value)
    )
  }

  # combine into table
  morerr.res <- dplyr::bind_rows(morerr.list)

  return(morerr.res)
}
