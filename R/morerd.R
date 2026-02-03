#' morerd: Multiple risk differences from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts, risks, and differences with associated CIs.
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
morerd <- function(x,
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
    utils::modifyList(formals(ezepi::morerd), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "evar", "ovar", "rexp", "iout", "rout"),
    utils::modifyList(formals(ezepi::morerd), as.list(match.call()[-1]))
  )

  # generate a table with totals
  morerd.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = TRUE,
    rate = FALSE,
    print = print
  )

  # calc risk differences and CIs to add to table
  morerd.list <- list()
  for(i in 1:nrow(morerd.df)){
    # calc risk diff
    morerd.calc <- ezepi::calc(
      "rd",
      morerd.df[i, 'case'][[1]],
      morerd.df[i, 'control'][[1]],
      morerd.df[morerd.df$exp == 'unexposed', 'case'][[1]],
      morerd.df[morerd.df$exp == 'unexposed', 'control'][[1]],
      conf_lvl = conf_lvl
    )

    # add row to list
    morerd.list[[i]] <- tibble::tibble_row(
      "exposure" = morerd.df$exp[[i]],
      "rd" = as.numeric(morerd.calc[1]),
      "lci" = as.numeric(morerd.calc[2]),
      "uci" = as.numeric(morerd.calc[3]),
      "p" = as.numeric(morerd.calc[4])
    )
  }

  # combine into table
  morerd.res <- dplyr::bind_rows(morerd.list)

  return(morerd.res)
}
