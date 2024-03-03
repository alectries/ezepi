#' moreir: nx2 table with case counts, person-time totals, and incidence rates from
#' categorical exposure and outcome variables and a person-time variable.
#'
#' Returns a tibble with stratified counts, person-time totals, and rates by
#' exposure status.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param person_time A variable in x reflecting the person-time contributed by each participant.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @return A tibble.
#' @export
moreir <- function(x,
                 exposure_var,
                 outcome_var,
                 person_time,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0
){
  # startup
  ## check that required vars exist
  if(
    is_empty(select({{x}}, {{exposure_var}}))
  ){
    stop("ezepi: Must specify exposure_var!")
  }
  if(
    is_empty(select({{x}}, {{outcome_var}}))
  ){
    stop("ezepi: Must specify outcome_var!")
  }
  if(
    is_empty(select({{x}}, {{person_time}}))
  ){
    stop("ezepi: Must specify person_time!")
  }
  ## pull vars
  test.exp <- x %>%
    pull({{exposure_var}})
  test.out <- x %>%
    pull({{outcome_var}})
  test.pt <- x %>%
    pull({{person_time}})
  test.df <- data.frame(test.exp, test.out, test.pt)
  ## tests
  if(
    class(test.exp) == class({{ref_exp}})
  ){
    message(paste0("ezepi: Referent exposure value is ", {{ref_exp}}))
  } else {
    stop("ezepi: Error: referent exposure does not match variable type")
  }
  if(
    test.df %>% filter(test.exp == {{ref_exp}}) %>% summarise(test.exp = n()) >= 1
  ){
    message("ezepi: Exposure variable set.")
  } else {
    stop("ezepi: Error: referent value does not exist in exposure_var")
  }
  if(
    class(test.out) == class({{index_out}}) &
    class(test.out) == class({{ref_out}})
  ){
    message(paste0("ezepi: Index outcome value is ", {{index_out}},
                   " and referent outcome value is ", {{ref_out}}))
  } else {
    stop("ezepi: Error: index/referent outcome does not match variable type")
  }
  if(
    test.df %>% filter(test.out == {{index_out}}) %>% summarise(test.out = n()) >= 1 &
    test.df %>% filter(test.out == {{ref_out}}) %>% summarise(test.out = n()) >= 1
  ){
    message("ezepi: Outcome variable set.")
  } else {
    stop("ezepi: Error: index/referent value does not exist in outcome_var")
  }
  if(
    !is.na(nth(test.pt, 1))
  ){
    message(paste0("ezepi: Person-time variable set."))
  } else {
    stop("ezepi: Error: person-time variable does not exist or is not set")
  }

  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           {{exposure_var}} != {{ref_exp}} & !is.na({{exposure_var}}) ~
                             paste0('exp.', {{exposure_var}}),
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate person-time table
  pt.df <- x.df %>%
    filter(!is.na(exp)) %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp) %>%
    select(!out) %>%
    summarise(pt = sum({{person_time}}))

  # generate counts table
  case.df <- x.df %>%
    filter(!is.na(exp)) %>%
    filter(out == 'case') %>%
    group_by(exp) %>%
    summarise(case = n())

  # join tables and calculate rates
  moreir.df <- case.df %>%
    inner_join(pt.df, by = join_by(exp)) %>%
    mutate(rate = case / pt)

  # add rows
  moreir.df <- moreir.df %>%
    ungroup() %>%
    add_row(
      exp = as.character('sum'),
      case = as.numeric(summarise(., sum(case))),
      pt = as.numeric(summarise(., sum(pt))),
      rate = as.numeric(case / pt)
    )

  # output
  return(moreir.df)
}
