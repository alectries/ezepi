#' moretbl: nx2 contingency table from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts by exposure and outcome, along with
#' sums and risks by exposure status. Like eztbl, but allows for more exposure
#' categories (but still binary outcome).
#'
#' Note: Before using moretbl, you must collapse all missing variables to NA.
#' All non-null values of exposure_var will be included in the analysis.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @export
moretbl <- function(x,
                  exposure_var,
                  outcome_var,
                  ref_exp = 0,
                  index_out = 1,
                  ref_out = 0
){
  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           !is.na({{exposure_var}}) & {{exposure_var}} != {{ref_exp}} ~
                             paste0('exp.', {{exposure_var}}),
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  moretbl.df <- x.df %>%
    filter(!is.na(exp)) %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(risk = case / total)

  # add rows
  moretbl.df <- moretbl.df %>%
    ungroup() %>%
    add_row(
      exp = as.character('sum'),
      case = as.numeric(summarise(., sum(case))),
      control = as.numeric(summarise(., sum(control))),
      total = as.numeric(summarise(., sum(total))),
      risk = as.numeric(case / total)
    )

  # output
  return(moretbl.df)
}
