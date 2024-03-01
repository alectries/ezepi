#' ezir: Table with case count, person-time totals, and incidence rates from
#' binary exposure and outcome variables and a person-time variable.
#'
#' Returns a tibble with stratified counts, person-time totals, and rates by
#' exposure status.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @export
ezir <- function(x,
                  exposure_var,
                  outcome_var,
                  person_time,
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

  # generate person-time table
  pt.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp) %>%
    select(!out) %>%
    summarise(pt = sum({{person_time}}))

  # generate counts table
  case.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case') %>%
    group_by(exp) %>%
    summarise(case = n())

  # join tables and calculate rates
  ezir.df <- case.df %>%
    inner_join(pt.df, by = join_by(exp)) %>%
    mutate(rate = case / pt)

  # add rows
  ezir.df <- ezir.df %>%
    ungroup() %>%
    add_row(
      exp = as.character('sum'),
      case = as.numeric(summarise(., sum(case))),
      pt = as.numeric(summarise(., sum(pt))),
      rate = as.numeric(case / pt)
    )

  # output
  return(ezir.df)
}
