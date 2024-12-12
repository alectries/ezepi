#' ezprev: Prevalence table from a binary outcome variable
#'
#' Returns a tibble with stratified counts by outcome, along with sums and
#' prevalence.
#'
#' @param x A dataset.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @importFrom utils modifyList
#' @export
ezprev <- function(x,
                   outcome_var,
                   index_out = 1,
                   ref_out = 0
){
  # startup
  ## startup function will go here later

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "ovar", "iout", "rout"),
    utils::modifyList(formals(ezepi::ezprev), as.list(match.call()[-1]))
  )

  # generate a table with totals
  ezprev.df <- ezepi:::table(
    x.df,
    index = TRUE,
    risk = NA,
    rate = FALSE
  )

  # output
  return(ezprev.df)
}
