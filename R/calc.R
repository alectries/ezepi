#' calc: Calculate measure of association from numeric inputs
#'
#' Returns a tibble with a p-value, 95 percent CI, and estimated measure of association.
#'
#' Recommended use: pipe to View("Title") for easiest viewing.
#'
#' calc always returns the Wald method, including on Mantel-Haenszel measures.
#'
#' @param calc The measure to be calculated. Choose from risk difference ("rd"), risk ratio ("rr"), incidence odds ratio ("ior"), incidence rate difference ("ird"), and incidence rate ratio ("irr"), or Mantel-Haenszel versions ("mhior", "mhrd", "mhrr").
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
#' @importFrom dplyr mutate
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
  # test that calc is possible
  if(!(calc %in% c("ior", "ird", "irr", "rd", "rr", "mhior", "mhrd", "mhrr"))){
    rlang::abort(message = c(
      cli::style_bold("Incorrect calc option selected!"),
      "i" = "Choose from:",
      "*" = paste0("ior: ",
                   cli::style_underline("I"), "ncidence ",
                   cli::style_underline("o"), "dds ",
                   cli::style_underline("r"), "atio"),
      "*" = paste0("ird: ",
                   cli::style_underline("I"), "ncidence ",
                   cli::style_underline("r"), "ate ",
                   cli::style_underline("d"), "ifference"),
      "*" = paste0("irr: ",
                   cli::style_underline("I"), "ncidence ",
                   cli::style_underline("r"), "ate ",
                   cli::style_underline("r"), "atio"),
      "*" = paste0("rd: ",
                   cli::style_underline("R"), "isk ",
                   cli::style_underline("d"), "ifference"),
      "*" = paste0("rr: ",
                   cli::style_underline("R"), "isk ",
                   cli::style_underline("r"), "atio"),
      "*" = paste0("mhior: ",
                   cli::style_underline("M"), "antel-",
                   cli::style_underline("H"), "aenszel ",
                   cli::style_underline("i"), "ncidence ",
                   cli::style_underline("o"), "dds ",
                   cli::style_underline("r"), "atio"),
      "*" = paste0("mhrd: ",
                   cli::style_underline("M"), "antel-",
                   cli::style_underline("H"), "aenszel ",
                   cli::style_underline("r"), "isk ",
                   cli::style_underline("d"), "ifference"),
      "*" = paste0("mhrr: ",
                   cli::style_underline("M"), "antel-",
                   cli::style_underline("H"), "aenszel ",
                   cli::style_underline("r"), "isk ",
                   cli::style_underline("r"), "atio"),
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
    calc.df <- tibble::as_tibble(a, .name_repair = "unique")
    names(calc.df) <- c("case", "control")
    problem <- FALSE
  }
  if(length(a) > 1 && length(a) == length(b) && length(a) == length(c) &&
     length(a) == length(d)){
    calc.df <- tibble::tibble(a = a, b = b, c = c, d = d)
    problem <- FALSE
  }
  if(problem){
    rlang::abort(
      message = c(
        cli::style_bold("Format is incorrect!"),
        "i" = "Ensure a is a 2x2 data frame or matrix, a-d are numbers, or a-d are same-length vectors for Mantel-Haenszel statistics."
      )
    )
  }

  # standardize n
  if(is.na(n) && !F %in% (names(calc.df) == c("case", "control"))){
    n <- sum(calc.df$case) + sum(calc.df$control)
  }
  if(is.na(n) && !F %in% (names(calc.df) == c("a", "b", "c", "d"))){
    n <- sum(calc.df$a, na.rm = T) + sum(calc.df$b, na.rm = T) +
      sum(calc.df$c, na.rm = T) + sum(calc.df$d, na.rm = T)
  }

  # calculate design effect if sumdiff exists
  if(exists("sumdiff") && !(calc %in% c("mhior", "mhrd", "mhrr"))){
    deff <- 1 + (1/n) * sumdiff /
      ((sum(calc.df$case) + sum(calc.df$control)) / n)^2
  }

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

  if(calc == "mhrd"){
    ## mutate values
    calc.mhrd <- mutate(
      calc.df,
      n = a + b + c + d,
      p1 = a / (a + b),
      p0 = c / (c + d),
      rd = p1 - p0,
      w = (a + b) * (c + d) / n,
      v = w^2 * (p1 * (1 - p1) / (a + b) + p0 * (1 - p0) / (c + d))
    )

    ## estimate
    estimate <- sum(calc.mhrd$rd * calc.mhrd$w, na.rm = T) /
      sum(calc.mhrd$w, na.rm = T)

    ## se
    se <- sqrt(sum(calc.mhrd$v, na.rm = T)) / sum(calc.mhrd$w, na.rm = T)

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

  if(calc == "mhrr"){
    ## mutate values
    calc.mhrr <- dplyr::mutate(
      calc.df,
      e1 = a * (c + d) / (a + b + c + d),
      e2 = c * (a + b) / (a + b + c + d),
      s1 = (a + c) * (a + b) * (c + d) / (a + b + c + d) ^ 2
    )

    ## estimate
    estimate <- sum(calc.mhrr$e1, na.rm = T) / sum(calc.mhrr$e2, na.rm = T)

    ## se
    se <- sqrt(
      sum(calc.mhrr$s1, na.rm = T) / sum(calc.mhrr$e1, na.rm = T) /
        sum(calc.mhrr$e2, na.rm = T)
    )

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

  if(calc == "mhior"){
    ## mutate values
    calc.mhior <- dplyr::mutate(
      calc.df,
      e1 = a * d / (a + b + c + d),
      e2 = b * c / (a + b + c + d),
      v = (b * c) / (a + b + c + d) ^ 2 * (1 / a + 1 / b + 1 / c + 1 / d)
    )

    ## estimate
    estimate <- sum(calc.mhior$e1, na.rm = T) / sum(calc.mhior$e2, na.rm = T)

    ## se
    se <- sqrt(
      sum(calc.mhior$v, na.rm = T) /
        sum(calc.mhior$e1, na.rm = T) /
        sum(calc.mhior$e2, na.rm = T)
    )

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
