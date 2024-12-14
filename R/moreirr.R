#' moreirr: Rate ratio from binary exposure and outcome variables
#'
#' Returns a tibble with stratified counts, rates, and rate ratios with associated CIs.
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
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom fmsb rateratio
#' @importFrom tibble tibble_row
#' @importFrom dplyr bind_rows
#' @export
moreirr <- function(x,
                  exposure_var,
                  outcome_var,
                  person_time,
                  ref_exp = 0,
                  index_out = 1,
                  ref_out = 0,
                  conf_lvl = 0.95
){
  # startup
  ezepi:::startup(
    c("xdat", "evar", "ovar", "ptim", "rexp", "iout", "rout", "clvl"),
    utils::modifyList(formals(ezepi::moreirr), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "ptim", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::moreirr), as.list(match.call()[-1]))
  )

  # generate a table with totals
  moreirr.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = NA,
    rate = TRUE
  )
  print(moreirr.df)

  # calc rate ratios and CIs to add to table
  moreirr.list <- list()
  for(i in 1:nrow(moreirr.df)){
    # calc rate ratio
    sink(file = nullfile())
    moreirr.fmsb <- fmsb::rateratio(
      moreirr.df[i, 'case'][[1]],
      moreirr.df[moreirr.df$exp == 'unexposed', 'case'][[1]],
      moreirr.df[i, 'pt'][[1]],
      moreirr.df[moreirr.df$exp == 'unexposed', 'pt'][[1]],
      conf.level = conf_lvl
    )
    sink()

    # add row to list
    moreirr.list[[i]] <- tibble::tibble_row(
      "Exposure" = moreirr.df$exp[[i]],
      "Rate Ratio" = as.numeric(moreirr.fmsb$estimate),
      "LCI" = as.numeric(moreirr.fmsb$conf.int[1]),
      "UCI" = as.numeric(moreirr.fmsb$conf.int[2]),
      "p-value" = as.numeric(moreirr.fmsb$p.value)
    )
  }

  # combine into table
  moreirr.res <- dplyr::bind_rows(moreirr.list)

  return(moreirr.res)
}
