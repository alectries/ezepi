#' eztbl: 2x2 contingency table from binary exposure and outcome variables
#'
#' Returns a tibble with stratified counts by exposure and outcome, along with
#' sums and risks by exposure status.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @export
eztbl <- function(x,
                  exposure_var,
                  outcome_var,
                  index_exp = 1,
                  ref_exp = 0,
                  index_out = 1,
                  ref_out = 0
){
  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{index_exp}} ~ 'exposed',
                           {{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  eztbl.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(risk = case / total)

  # add rows
  eztbl.df <- eztbl.df %>%
    ungroup() %>%
    add_row(
      exp = as.character('sum'),
      case = as.numeric(summarise(., sum(case))),
      control = as.numeric(summarise(., sum(control))),
      total = as.numeric(summarise(., sum(total))),
      risk = as.numeric(case / total)
    )

  # output
  return(eztbl.df)
}
