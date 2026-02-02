#' calc: Calculate measure of association from numeric inputs
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated measure of association.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' @param calc The measure to be calculated. Choose from prevalence ("prev"), risk difference ("rd"), risk ratio ("rr"), incidence odds ratio ("ior"), incidence rate difference ("ird"), and incidence rate ratio ("irr").
#' @param a The number of exposed cases. Alternatively, a 2x2 dataframe or matrix, in format data.frame(c(a, c), c(b, d)) or matrix(c(a, c, b, d)), of counts.
#' @param b The number of exposed controls (or exposed person-time). Alternatively, if a is a dataframe or matrix, NA.
#' @param c The number of unexposed cases. Alternatively, if a is a dataframe or matrix, NA.
#' @param d The number of unexposed controls (or unexposed person-time). Alternatively, if a is a dataframe or matrix, NA.
#' @param n If a-d are actual counts of real persons (or person-time), NA. If a-d are sums of weights, n is the total number of actual persons sampled.
#' @param sumdiff If a-d are actual counts of real persons (or person-time), NA. If a-d are sums of weights, sumdiff is the sum of the squared differences between each observation's weight and the mean weight.
#' @param conf_lvl The preferred confidence level for hypothesis testing. Defaults to 0.95.
#' @return A tibble.
#' @importFrom utils modifyList
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr rename
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @export
calc <- function(calc,
                 a,
                 b = NA,
                 c = NA,
                 d = NA,
                 n = NA,
                 sumdiff = NA,
                 conf_lvl = 0.95
){
  # startup
  ezepi:::startup(
    c("clvl"),
    utils::modifyList(formals(ezepi::calc), as.list(match.call()[-1]))
  )

  # test that calc is possible
  if(!(calc %in% c("ior", "ird", "irr", "rd", "rr"))){
    rlang::abort(message = c(
      cli::style_bold("Incorrect calc option selected!"),
      "i" = "Choose from:",
      "*" = paste0("ior: ",
                   cli::style_underline("I"), "ncidence ",
                   cli::style_underline("o"), "dds",
                   cli::style_underline("r"), "atio"),
      "*" = paste0("ird: ",
                   cli::style_underline("I"), "ncidence ",
                   cli::style_underline("r"), "ate",
                   cli::style_underline("d"), "ifference"),
      "*" = paste0("irr: ",
                   cli::style_underline("I"), "ncidence ",
                   cli::style_underline("r"), "ate",
                   cli::style_underline("r"), "atio"),
      "*" = paste0("rd: ",
                   cli::style_underline("R"), "isk",
                   cli::style_underline("d"), "ifference"),
      "*" = paste0("rr: ",
                   cli::style_underline("R"), "isk",
                   cli::style_underline("r"), "atio")
    ))
  }

  # standardize 2x2 table
  problem <- TRUE
  if(length(a) == 1 & is.numeric(a)){
    calc.df <- tibble::tibble(
      case = c(as.numeric(a), as.numeric(c)),
      control = c(as.numeric(b), as.numeric(d))
    )
    problem <- FALSE
  }
  if(length(a) == 2 & is.data.frame(a)){
    calc.df <- tibble::as_tibble(a)
    names(calc.df) <- c("case", "control")
    problem <- FALSE
  }
  if(length(a) == 4 & is.matrix(a)){
    calc.df <- tibble::as_tibble(.name_repair = "unique")
    names(calc.df) <- c("case", "control")
    problem <- FALSE
  }
  if(problem){
    rlang::abort(
      message = c(
        cli::style_bold("Format is incorrect!"),
        "i" = "Ensure a is a 2x2 data frame or matrix, or a-d are numbers."
      )
    )
  }

  # standardize n
  if(is.na(n)){
    n <- sum(calc.df$case) + sum(calc.df$control)
  }

  # calculate design effect if sumdiff exists
  deff <- 1 + sqrt(
    sumdiff / n /
      ((sum(calc.df$case) + sum(calc.df$control)) / n)
  )^2

  # calculate result
  if(calc == "ior"){
    ## estimate
    estimate <- calc.df$case[[1]] * calc.df$control[[2]] /
      (calc.df$control[[1]] * calc.df$case[[2]])

    ## standard error
    if(is.na(sumdiff)){
      se <- sqrt(
        (1 / calc.df$case[[1]]) +
          (1 / calc.df$control[[1]]) +
          (1 / calc.df$case[[2]]) +
          (1 / calc.df$control[[2]])
      )
    } else {
      se <- deff *
        sqrt(
          (1 / calc.df$case[[1]]) +
            (1 / calc.df$control[[1]]) +
            (1 / calc.df$case[[2]]) +
            (1 / calc.df$control[[2]])
        )
    }

    ## lci
    lci <- exp(
      log(estimate) -
        qt(1 - (1 - conf_lvl)/2, df = n) *
        se
    )

    ## uci
    uci <- exp(
      log(estimate) +
        qt(1 - (1 - conf_lvl)/2, df = n) *
        se
    )

    ## p-value
    p <- 2 * (1 - pnorm(abs(log(estimate) / se)))

    ## return
    return(c(estimate = estimate, lci = lci, uci = uci, p = p))
  }

  if(calc == "ird"){
    ## estimate
    estimate <- calc.df$case[[1]] / calc.df$control[[1]] -
      calc.df$case[[2]] / calc.df$control[[2]]

    ## standard error
    if(is.na(sumdiff)){
      se <- sqrt(
        calc.df$case[[1]] / calc.df$control[[1]]^2 +
          calc.df$case[[2]] / calc.df$control[[2]]^2
      )
    } else {
      rlang::abort(message = c(
        cli::style_bold("Weighting unavailable for incidence rates."),
        "x" = "ezepi cannot currently calculate this measure."
      ))
    }

    ## lci
    lci <- estimate -
      qt(1 - (1 - conf_lvl)/2, df = n) *
      se

    ## uci
    uci <- estimate +
      qt(1 - (1 - conf_lvl)/2, df = n) *
      se

    ## p-value
    p <- 2 * (1 - pnorm(abs(estimate / se)))

    ## return
    return(c(estimate = estimate, lci = lci, uci = uci, p = p))
  }

  if(calc == "irr"){
    ## estimate
    estimate <- calc.df$case[[1]] / calc.df$control[[1]] /
      (calc.df$case[[2]] / calc.df$control[[2]])

    ## standard error
    if(is.na(sumdiff)){
      se <- sqrt(
        1 / calc.df$case[[1]] +
          1 / calc.df$case[[2]]
      )
    } else {
      rlang::abort(message = c(
        cli::style_bold("Weighting unavailable for incidence rates."),
        "x" = "ezepi cannot currently calculate this measure."
      ))
    }

    ## lci
    lci <- exp(
      log(estimate) -
        qt(1 - (1 - conf_lvl)/2, df = n) *
        se
    )

    ## uci
    uci <- exp(
      log(estimate) +
        qt(1 - (1 - conf_lvl)/2, df = n) *
        se
    )

    ## p-value
    p <- 2 * (1 - pnorm(abs(log(estimate) / se)))

    ## return
    return(c(estimate = estimate, lci = lci, uci = uci, p = p))
  }

  if(calc == "rd"){
    ## estimate
    estimate <- calc.df$case[[1]] / (calc.df$case[[1]] + calc.df$control[[1]]) -
      calc.df$case[[2]] / (calc.df$case[[2]] + calc.df$control[[2]])

    ## standard error
    if(is.na(sumdiff)){
      se <- sqrt(
        calc.df$case[[1]] / (calc.df$case[[1]] + calc.df$control[[1]]) *
          (1 - (calc.df$case[[1]] / (calc.df$case[[1]] + calc.df$control[[1]]))) /
          (calc.df$case[[1]] + calc.df$control[[1]]) +
          calc.df$case[[2]] / (calc.df$case[[2]] + calc.df$control[[2]]) *
          (1 - (calc.df$case[[2]] / (calc.df$case[[2]] + calc.df$control[[2]]))) /
          (calc.df$case[[2]] + calc.df$control[[2]])
      )
    } else {
      se <- deff *
        sqrt(
          calc.df$case[[1]] / (calc.df$case[[1]] + calc.df$control[[1]]) *
            (1 - (calc.df$case[[1]] / (calc.df$case[[1]] + calc.df$control[[1]]))) /
            (calc.df$case[[1]] + calc.df$control[[1]]) +
            calc.df$case[[2]] / (calc.df$case[[2]] + calc.df$control[[2]]) *
            (1 - (calc.df$case[[2]] / (calc.df$case[[2]] + calc.df$control[[2]]))) /
            (calc.df$case[[2]] + calc.df$control[[2]])
        )
    }

    ## lci
    lci <- estimate -
      qt(1 - (1 - conf_lvl)/2, df = n) *
      se

    ## uci
    uci <- estimate +
      qt(1 - (1 - conf_lvl)/2, df = n) *
      se

    ## p-value
    p <- 2 * (1 - pnorm(abs(estimate / se)))

    ## return
    return(c(estimate = estimate, lci = lci, uci = uci, p = p))
  }

  if(calc == "rr"){
    ## estimate
    estimate <- calc.df$case[[1]] / (calc.df$case[[1]] + calc.df$control[[1]]) /
      (calc.df$case[[2]] / (calc.df$case[[2]] + calc.df$control[[2]]))

    ## standard error
    if(is.na(sumdiff)){
      se <- sqrt(
        1 / calc.df$case[[1]] -
          1 / (calc.df$case[[1]] + calc.df$control[[1]]) +
          1 / calc.df$case[[2]] -
          1 / (calc.df$case[[2]] + calc.df$control[[2]])
      )
    } else {
      se <- deff *
        sqrt(
          1 / calc.df$case[[1]] -
            1 / (calc.df$case[[1]] + calc.df$control[[1]]) +
            1 / calc.df$case[[2]] -
            1 / (calc.df$case[[2]] + calc.df$control[[2]])
        )
    }

    ## lci
    lci <- exp(
      log(estimate) -
        qt(1 - (1 - conf_lvl)/2, df = n) *
        se
    )

    ## uci
    uci <- exp(
      log(estimate) +
        qt(1 - (1 - conf_lvl)/2, df = n) *
        se
    )

    ## p-value
    p <- 2 * (1 - pnorm(abs(log(estimate) / se)))

    ## return
    return(c(estimate = estimate, lci = lci, uci = uci, p = p))
  }
}
