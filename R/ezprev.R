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
#' @export
ezprev <- function(x,
                   outcome_var,
                   index_out = 1,
                   ref_out = 0
){
  # standardize data
  x.df <- x %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  ezprev.df <- x.df %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(prevalence = case / total)

  # output
  return(ezprev.df)
}
