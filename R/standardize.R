#' .standardize: Standardize variables
#'
#' Not exported. Takes variables from original data and moves them to exposure and outcome variables.
#'
#' @param args A string containing information about the arguments passed to the main function.
#' @param list A list of arguments passed to the main function.
#' @importFrom stringr str_detect
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @importFrom dplyr pull
#' @keywords internal

standardize <- function(args, list){
  # Read args
  xdat <- "xdat" %in% args
  evar <- "evar" %in% args
  ovar <- "ovar" %in% args
  ptim <- "ptim" %in% args
  iexp <- "iexp" %in% args
  rexp <- "rexp" %in% args
  iout <- "iout" %in% args
  rout <- "rout" %in% args

  # Standardize
  if(!xdat){
    rlang::abort(
      message = c(
        "x not used in this function.",
        i = "What are you trying to standardize?"
      )
    )
  } else {
    # Pull dataframe from list
    x <- eval(list$x)

    # Initialize standardized df
    x.df <- tibble::tibble(.rows = nrow(x))

    # Pull exposure_var
    if(evar){x.df$exp <- paste0(dplyr::pull(x, list$exposure_var))}

    # Pull outcome_var
    if(ovar){x.df$out <- paste0(dplyr::pull(x, list$outcome_var))}

    # Pull person_time
    if(ptim){x.df$pt <- dplyr::pull(x, list$person_time)}

    # Mutate index exposure (to "exposed")
    if(iexp){
      x.df$exp <- ifelse(
        x.df$exp == paste0(list$index_exp),
        "exposed",
        x.df$exp
      )
    }

    # Mutate non-referent exposures
    if(rexp & !iexp){
      x.df$exp <- ifelse(
        !is.na(x.df$exp) & x.df$exp != paste0(list$ref+exp),
        paste0("exp.", x.df$exp),
        x.df$exp
      )
    }

    # Mutate referent exposures (to "unexposed")
    if(rexp){
      x.df$exp <- ifelse(
        x.df$exp == paste0(list$ref_exp),
        "unexposed",
        x.df$exp
      )
    }

    # Remove all other exposures
    if(iexp | rexp){
      x.df$exp <- ifelse(
        stringr::str_detect(x.df$exp, "exposed|exp.|unexposed"),
        x.df$exp,
        NA
      )
    }

    # Mutate index outcomes (to "case")
    if(iout){
      x.df$out <- ifelse(
        x.df$out == paste0(list$index_out),
        "case",
        x.df$out
      )
    }

    # Mutate referent outcomes (to "control")
    if(rout){
      x.df$out <- ifelse(
        x.df$out == paste0(list$ref_out),
        "control",
        x.df$out
      )
    }

    # Remove all other outcomes
    if(iout | rout){
      x.df$out <- ifelse(
        stringr::str_detect(x.df$out, "case|control"),
        x.df$out,
        NA
      )
    }

    # Return
    return(x.df)
  }
}
