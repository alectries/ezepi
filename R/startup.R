#' startup: Initial sanity checking
#'
#' Not exported. Checks inputs for problems, halting execution as needed.
#'
#' @param args A string containing information about the arguments passed to the main function.
#' @param list A list of arguments passed to the main function.
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom rlang inform
#' @importFrom rlang caller_env
#' @importFrom cli style_bold
#' @importFrom dplyr pull
#' @importFrom magrittr `%>%`
#' @keywords internal

startup <- function(args, list){
  # Read args
  xdat <- "xdat" %in% args
  evar <- "evar" %in% args
  ovar <- "ovar" %in% args
  ptim <- "ptim" %in% args
  iexp <- "iexp" %in% args
  rexp <- "rexp" %in% args
  iout <- "iout" %in% args
  rout <- "rout" %in% args
  clvl <- "clvl" %in% args
  rwnm <- "rwnm" %in% args
  numd <- "numd" %in% args
  risk <- "risk" %in% args

  # xdat
  if(xdat){
    # Does dataset exist? If yes, eval
    tryCatch(
      {
        x <- if(as.character(list$x) == ".") {
          get(".", envir = parent.frame(n=2))
          } else {
            get(as.character(list$x))
            }
        },
      error = function(cond){rlang::abort(
        message = c(
          cli::style_bold("x does not exist!"),
          "i" = "Did you forget to specify a dataset?"
        ),
        call = rlang::caller_env(5)
      )}
    )

    # Is x a dataset?
    if(!("data.frame" %in% class(x))){
      rlang::abort(
        message = c(
          cli::style_bold("x is not a dataset!"),
          "i" = "Did you forget to pipe?"
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  # evar
  if(evar){
    # Does variable exist? If yes, pull
    tryCatch(
      {exposure_var <- dplyr::pull(x, list$exposure_var)},
      ## fail if pull fails
      error = function(cond){rlang::abort(
        message = c(
          cli::style_bold("Variable does not exist!"),
          "x" = paste0(
            ifelse(paste0(list$exposure_var) == "",
                   "exposure_var",
                   paste0(list$exposure_var)),
            " does not exist in ",
            list$x,
            "."
          ),
          "i" = "Did you forget to specify exposure_var?"
        ),
        call = rlang::caller_env(5)
      )}
    )
  }

  # ovar
  if(ovar){
    # Does variable exist? If yes, pull
    tryCatch(
      {outcome_var <- dplyr::pull(x, list$outcome_var)},
      ## fail if pull fails
      error = function(cond){rlang::abort(
        message = c(
          cli::style_bold("Variable does not exist!"),
          "x" = paste0(
            ifelse(paste0(list$outcome_var) == "",
                   "outcome_var",
                   paste0(list$outcome_var)),
            " does not exist in ",
            list$x,
            "."
          ),
          "i" = "Did you forget to specify outcome_var?"
        ),
        call = rlang::caller_env(5)
      )}
    )
  }

  # ptim
  if(ptim){
    # Does variable exist? If yes, pull
    tryCatch(
      {person_time <- dplyr::pull(x, list$person_time)},
      ## fail if pull fails
      error = function(cond){rlang::abort(
        message = c(
          cli::style_bold("Variable does not exist!"),
          "x" = paste0(
            ifelse(paste0(list$person_time) == "",
                   "person_time",
                   paste0(list$person_time)),
            " does not exist in ",
            list$x,
            "."
          ),
          "i" = "Did you forget to specify person_time?"
        ),
        call = rlang::caller_env(5)
      )}
    )

    # Is person_time numeric?
    if(!is.numeric(person_time)){
      rlang::abort(
        message = c(
          cli::style_bold("person_time must be numeric!"),
          "x" = paste0(list$person_time, " is not a numeric variable."),
          "i" = paste0("Try as.numeric(", list$person_time, ").")
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  # iexp
  if(iexp){
    # Does value exist in variable?
    if(!(list$index_exp %in% exposure_var)){
      rlang::abort(
        message = c(
          cli::style_bold("Value does not exist!"),
          "x" = paste0(list$index_exp, " does not exist in ", list$exposure_var, "."),
          "i" = "Try adding or removing quotes."
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  # rexp
  if(rexp){
    # Does value exist in variable?
    if(!(list$ref_exp %in% exposure_var)){
      rlang::abort(
        message = c(
          cli::style_bold("Value does not exist!"),
          "x" = paste0(list$ref_exp, " does not exist in ", list$exposure_var, "."),
          "i" = "Try adding or removing quotes."
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  # iout
  if(iout){
    # Does value exist in variable?
    if(!(list$index_out %in% outcome_var)){
      rlang::abort(
        message = c(
          cli::style_bold("Value does not exist!"),
          "x" = paste0(list$index_out, " does not exist in ", list$outcome_var, "."),
          "i" = "Try adding or removing quotes."
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  # rout
  if(rout){
    # Does value exist in variable?
    if(!(list$ref_out %in% outcome_var)){
      rlang::abort(
        message = c(
          cli::style_bold("Value does not exist!"),
          "x" = paste0(list$ref_out, " does not exist in ", list$outcome_var, "."),
          "i" = "Try adding or removing quotes."
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  # clvl
  if(clvl){
    # Is value numeric?
    if(!is.numeric(list$conf_lvl)){
      rlang::abort(
        message = c(
          cli::style_bold("Confidence level must be numeric!"),
          "x" = paste0(list$conf_lvl, " is not numeric."),
          "i" = paste0("Try as.numeric(", list$conf_lvl, ").")
        ),
        call = rlang::caller_env(1)
      )
    }

    # Is value 0.95?
    if(list$conf_lvl != 0.95){
      rlang::inform(
        paste0(cli::style_bold("ezepi:"), "Using ", list$conf_lvl, " confidence.")
      )
    }
  }

  # rwnm
  if(rwnm){
    ## only activate if row_name is set
    if(!is.null(row_name)){
      # Is row_name unique?
      if(row_name %in% dplyr::pull(list$x, 1)){
        rlang::abort(
          message = c(
            cli::style_bold("Row name must be unique!"),
            "x" = paste0(list$row_name, " is already in ", names(x)[1], "."),
            "i" = "Try a different name."
          ),
          call = rlang::caller_env(1)
        )
      }
    }
  }

  # numd
  if(numd){
    # Get value
    numeric_data <- get0(
      "list$numeric_data",
      ifnotfound = get0("list$.numeric_data")
    )

    ## only activate if numeric_data is TRUE
    if(numeric_data){
      # Are the non-name columns numeric?
      if(!all(sapply(x[-1], is.numeric))){
        rlang::abort(
          message = c(
            cli::style_bold("Data is non-numeric!"),
            "x" = paste0("At least one column in ", list$x, " is non-numeric."),
            "i" = "Try changing numeric_data to FALSE."
          ),
          call = rlang::caller_env(1)
        )
      }
    }
  }

  # risk
  if(risk){
    if(!is.logical(list$risk)){
      rlang::abort(
        message = c(
          cli::style_bold("Risk must be logical!"),
          "x" = paste0("risk cannot be ", list$risk, "."),
          "i" = "Set risk to TRUE or FALSE."
        ),
        call = rlang::caller_env(1)
      )
    }
  }

  return("")
}
