#' moreior: Multiple odds ratios from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts, odds, and odds ratios with associated CIs.
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
#' @importFrom tibble tibble_row
#' @importFrom dplyr bind_rows
#' @export
moreior <- function(x,
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
    utils::modifyList(formals(ezepi::moreior), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::moreior), as.list(match.call()[-1]))
  )

  # generate a table with totals
  moreior.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = FALSE,
    rate = FALSE,
    print = print
  )

  # calc odds ratios and CIs to add to table
  moreior.list <- list()
  for(i in 1:nrow(moreior.df)){
    # calc odds ratio
    moreior.calc <- ezepi::calc(
      "ior",
      moreior.df[i, 'case'][[1]],
      moreior.df[i, 'control'][[1]],
      moreior.df[moreior.df$exp == 'unexposed', 'case'][[1]],
      moreior.df[moreior.df$exp == 'unexposed', 'control'][[1]],
      conf_lvl = conf_lvl
    )

    # add row to list
    moreior.list[[i]] <- tibble::tibble_row(
      "exposure" = moreior.df$exp[[i]],
      "ior" = as.numeric(moreior.calc[1]),
      "lci" = as.numeric(moreior.calc[2]),
      "uci" = as.numeric(moreior.calc[3]),
      "p" = as.numeric(moreior.calc[4])
    )
  }

  # combine into table
  moreior.res <- dplyr::bind_rows(moreior.list)

  return(moreior.res)
}
