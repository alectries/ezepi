#' moreprev: Prevalence table from a categorical outcome variable
#'
#' Returns a tibble with stratified counts by outcome, along with sums and
#' prevalence.
#'
#' @param x A dataset.
#' @param outcome_var A categorical outcome variable in x.
#' @return A tibble.
#' @importFrom utils modifyList
#' @export
moreprev <- function(x,
                     outcome_var
){
  # startup
  ezepi:::startup(
    c("xdat", "ovar"),
    utils::modifyList(formals(ezepi::moreprev), as.list(match.call()[-1]))
  )

  # standardize data
  x.df <- ezepi:::standardize(
    c("xdat", "ovar"),
    utils::modifyList(formals(ezepi::moreprev), as.list(match.call()[-1]))
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
