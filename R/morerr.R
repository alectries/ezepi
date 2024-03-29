#' morerr: Multiple risk ratios from categorical exposure and outcome variables
#'
#' Returns a tibble with stratified counts, risks, and ratios with associated CIs.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param x A dataset.
#' @param exposure_var A categorical exposure variable in x.
#' @param outcome_var A categorical outcome variable in x.
#' @param strat_var A categorical variable in x by which to stratify estimates.
#' @param ref_exp The value of exposure_var to treat as the unexposed group. Defaults to 0.
#' @param index_out The value of outcome_var to treat as cases. Defaults to 1.
#' @param ref_out The value of outcome_var to treat as non-cases. Defaults to 0.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @param mh Sets whether to use (Cochran-)Mantel-Haenszel pooling. 1 for yes, defaults to 0.
#' @return A tibble.
#' @export
morerr <- function(x,
                   exposure_var,
                   outcome_var,
                   strat_var = NULL,
                   ref_exp = 0,
                   index_out = 1,
                   ref_out = 0,
                   conf_lvl = 0.95,
                   mh = 0
){
  # startup
  ## check that required packages are loaded
  if("dplyr" %in% (.packages())){} else {
    stop("ezepi: ezepi requires the tidyverse. Please execute library(tidyverse) or library(ezepi) before continuing.")
  }
  if("fmsb" %in% (.packages())){} else {
    stop("ezepi: ezepi requires fmsb. Please execute library(fmsb) or library(ezepi) before continuing.")
  }
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
    hasArg(strat_var)
  ){
    if(is_empty(select({{x}}, {{strat_var}}))){
      stop("ezepi: strat_var must exist in dataset!")
    } else {
      message("ezepi: Stratification variable set.")
    }
  }

  ## pull vars
  test.exp <- x %>%
    pull({{exposure_var}})
  test.out <- x %>%
    pull({{outcome_var}})
  test.df <- data.frame(test.exp, test.out)
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
    mh == 1
  ){
    if(hasArg(strat_var)){
      message("ezepi: Using MH pooling.")
    } else {
      stop("ezepi: Error: must set strat_var to use MH")
    }
  } else {
    if(mh != 0){
      stop("ezepi: Error: mh must be set to 0 or 1")
    }
  }

  # standardize data
  x.df <- x %>%
    mutate(exp = case_when({{exposure_var}} == {{ref_exp}} ~ 'unexposed',
                           !is.na({{exposure_var}}) & {{exposure_var}} != {{ref_exp}} ~
                             paste0('exp.', {{exposure_var}}),
                           .default = NA)) %>%
    mutate(out = case_when({{outcome_var}} == {{index_out}} ~ 'case',
                           {{outcome_var}} == {{ref_out}} ~ 'control',
                           .default = NA))

  # add stratification variable if necessary
  if(
    hasArg(strat_var)
  ){
    x.df <- x.df %>%
      mutate(strat = case_when(!is.na({{strat_var}}) ~ paste0('st.', {{strat_var}})))
  }

  # if strat is not set
  if(!hasArg(strat_var)){

  # generate a table with totals
  morerr.df <- x.df %>%
    filter(!is.na(exp)) %>%
    filter(out == 'case' | out == 'control') %>%
    group_by(exp, out) %>%
    tally() %>%
    spread(out, n) %>%
    mutate(total = rowSums(across(everything()))) %>%
    mutate(risk = case / total)

  # calc risk ratios and CIs to add to table
  morerr.res <- morerr.df %>%
    # RR estimate
    mutate(RR = as.numeric(fmsb::riskratio(
      case,
      morerr.df %>%
        filter(exp == 'unexposed') %>%
        pull(case),
      total,
      morerr.df %>%
        filter(exp == 'unexposed') %>%
        pull(total),
      conf.level = conf_lvl
    ) %>% unlist() %>% unname() %>%
      data.frame(name = c('p.value', 'conf.int1', 'conf.int2', 'estimate',
                          'method', 'data.name'), value = .) %>%
      filter(name == 'estimate') %>%
      pull(value))) %>%
    # Lower CI for RR
    mutate(LCI = case_when(exp != 'unexposed' ~ as.numeric(fmsb::riskratio(
      case,
      morerr.df %>%
        filter(exp == 'unexposed') %>%
        pull(case),
      total,
      morerr.df %>%
        filter(exp == 'unexposed') %>%
        pull(total),
      conf.level = conf_lvl
    ) %>% unlist() %>% unname() %>%
      data.frame(name = c('p.value', 'conf.int1', 'conf.int2', 'estimate',
                          'method', 'data.name'), value = .) %>%
      filter(name == 'conf.int1') %>%
      pull(value)),
    exp == 'unexposed' ~ 0)) %>%
    # Upper CI for RR
    mutate(UCI = case_when(exp != 'unexposed' ~ as.numeric(fmsb::riskratio(
      case,
      morerr.df %>%
        filter(exp == 'unexposed') %>%
        pull(case),
      total,
      morerr.df %>%
        filter(exp == 'unexposed') %>%
        pull(total),
      conf.level = conf_lvl
    ) %>% unlist() %>% unname() %>%
      data.frame(name = c('p.value', 'conf.int1', 'conf.int2', 'estimate',
                          'method', 'data.name'), value = .) %>%
      filter(name == 'conf.int2') %>%
      pull(value)),
    exp == 'unexposed' ~ 0))
  } else {

    # if strat_var is set, but mh is off
    if(hasArg(strat_var) & mh == 0){

      # generate a table with totals
      morerr.df <- x.df %>%
        filter(!is.na(strat)) %>%
        filter(!is.na(exp)) %>%
        filter(out == 'case' | out == 'control') %>%
        group_by(strat, exp, out) %>%
        tally() %>%
        spread(out, n) %>%
        mutate(total = rowSums(across(everything()))) %>%
        mutate(risk = case / total)

      # calc risk ratios and CIs to add to table
      morerr.res <- morerr.df %>%
        # RR estimate
        mutate(RR = as.numeric(fmsb::riskratio(
          case,
          morerr.df %>%
            filter(strat == strat) %>%
            filter(exp == 'unexposed') %>%
            pull(case),
          total,
          morerr.df %>%
            filter(strat == strat) %>%
            filter(exp == 'unexposed') %>%
            pull(total),
          conf.level = conf_lvl
        ) %>% unlist() %>% unname() %>%
          nth(4)
          )) %>%
        # Lower CI for RR
        mutate(LCI = case_when(exp != 'unexposed' ~ as.numeric(fmsb::riskratio(
          case,
          morerr.df %>%
            filter(strat == strat) %>%
            filter(exp == 'unexposed') %>%
            pull(case),
          total,
          morerr.df %>%
            filter(strat == strat) %>%
            filter(exp == 'unexposed') %>%
            pull(total),
          conf.level = conf_lvl
        ) %>% unlist() %>% unname() %>%
          nth(2)
          ),
        exp == 'unexposed' ~ 0)) %>%
        # Upper CI for RR
        mutate(UCI = case_when(exp != 'unexposed' ~ as.numeric(fmsb::riskratio(
          case,
          morerr.df %>%
            filter(strat == strat) %>%
            filter(exp == 'unexposed') %>%
            pull(case),
          total,
          morerr.df %>%
            filter(strat == strat) %>%
            filter(exp == 'unexposed') %>%
            pull(total),
          conf.level = conf_lvl
        ) %>% unlist() %>% unname() %>%
          nth(3)
          ),
        exp == 'unexposed' ~ 0))
    } else {

      # if strat_var is set and mh is on
      if(hasArg(strat_var) & mh == 1){

        # get number of rows
        length <- length(select({{x}}, {{strat_var}}))
        counter <- length

        # generate a table with totals
        morerr.df <- x.df %>%
          filter(!is.na(strat)) %>%
          filter(!is.na(exp)) %>%
          filter(out == 'case' | out == 'control') %>%
          group_by(strat, exp, out) %>%
          tally() %>%
          spread(out, n) %>%
          mutate(total = rowSums(across(everything()))) %>%
          mutate(risk = case / total)

        # calc risk ratios and CIs to add to table
        morerr.res <- morerr.df %>%
          mutate(RRMH = as.numeric(fmsb::RRMH(matrix(
            data = c(paste0(
              repeat{
                # decrease counter by one
                counter <- counter - 1

                # set values
                col1 <- case
                col2 <- morerr.df %>%
                  filter(strat == strat) %>%
                  filter(exp == 'unexposed') %>%
                  select(case) %>%
                  pull()
                col3 <- total
                col4 <- morerr.df %>%
                  filter(strat == strat) %>%
                  filter(exp == 'unexposed') %>%
                  select(total) %>%
                  pull()

                # collect and output values
                if(counter != 0){
                  paste0(col1, ", ", col2, ", ", col3, ", ", col4, ", ") %>%
                    invisible()
                } else {
                  paste0(col1, ", ", col2, ", ", col3, ", ", col4) %>%
                    invisible()
                  break
                }
              })),
            nrow = length,
            ncol = 4
          ),
          conf.level = conf_lvl)
          ))
      }
    }
  }


  return(morerr.res)
}
