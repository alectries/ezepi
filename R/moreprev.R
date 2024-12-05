#' moreprev: Prevalence table from a categorical outcome variable
#'
#' Returns a tibble with stratified counts by outcome, along with sums and
#' prevalence.
#'
#' @param x A dataset.
#' @param outcome_var A categorical outcome variable in x.
#' @return A tibble.
#' @export
moreprev <- function(x,
                     outcome_var
){
  # startup
  ## startup function will go here later

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "ovar"),
    modifyList(formals(ezepi::moreprev), as.list(match.call()[-1]))
  )

  # generate a table
  moreprev.df <- ezepi:::table(
    x.df,
    index = FALSE,
    risk = NA,
    rate = FALSE
  )

  # output
  return(moreprev.df)
}
