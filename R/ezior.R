#' ezior: Odds ratio from binary exposure and outcome variables
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated incidence
#' odds ratio.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param index_exp The value of exposure_var to treat as the exposed group. Defaults to 1.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @export
ezior <- function(x,
                 exposure_var,
                 outcome_var,
                 index_exp = 1,
                 ref_exp = 0,
                 index_out = 1,
                 ref_out = 0,
                 conf_lvl = 0.95
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
  ## pull vars
  test.exp <- x %>%
    pull({{exposure_var}})
  test.out <- x %>%
    pull({{outcome_var}})
  test.df <- data.frame(test.exp, test.out)
  ## tests
  if(
    class(test.exp) == class({{index_exp}}) &
    class(test.exp) == class({{ref_exp}})
  ){
    message(paste0("ezepi: Index exposure value is ", {{index_exp}},
                   " and referent exposure value is ", {{ref_exp}}))
  } else {
    stop("ezepi: Error: index/referent exposure does not match variable type")
  }
  if(
    test.df %>% filter(test.exp == {{index_exp}}) %>% summarise(test.exp = n()) >= 1 &
    test.df %>% filter(test.exp == {{ref_exp}}) %>% summarise(test.exp = n()) >= 1
  ){
    message("ezepi: Exposure variable set.")
  } else {
    stop("ezepi: Error: index/referent value does not exist in exposure_var")
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

  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{index_exp}} ~ 'exposed',
                           {{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # generate a table with totals
  ezior.df <- x.df %>%
    filter(exp == 'exposed' | exp == 'unexposed') %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(risk = case / total)

  # calc odds ratio from table
  ezior.fmsb <- fmsb::oddsratio(
    ezior.df %>%
      ungroup() %>%
      filter(exp == 'exposed') %>%
      select(case) %>%
      pull(),
    ezior.df %>%
      ungroup() %>%
      filter(exp == 'unexposed') %>%
      select(case) %>%
      pull(),
    ezior.df %>%
      ungroup() %>%
      filter(exp == 'exposed') %>%
      select(control) %>%
      pull(),
    ezior.df %>%
      ungroup() %>%
      filter(exp == 'unexposed') %>%
      select(control) %>%
      pull(),
    conf.level = conf_lvl
  )

  # pull numbers from fmsb for tibble
  ezior.res <- as_tibble(data.frame(
    item = c(
      "Odds Ratio", "LCI", "UCI", "p-value"
    ),
    result = c(
      ezior.fmsb %>% unlist() %>% unname() %>% nth(4) %>% as.numeric(),
      ezior.fmsb %>% unlist() %>% unname() %>% nth(2) %>% as.numeric(),
      ezior.fmsb %>% unlist() %>% unname() %>% nth(3) %>% as.numeric(),
      ezior.fmsb %>% unlist() %>% unname() %>% nth(1) %>% as.numeric()
    )
  ))

  return(ezior.res)
}
