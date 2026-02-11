# ============================
# Auxiliary stats functions
# ============================

#' @keywords internal
.format_pval <- function(p, digits = 3, sci_cut = 1e-3) {

  if (is.na(p)) return(NA_character_)

  if (p < .Machine$double.eps) {
    return("< 2.2e-16")
  }

  if (p < sci_cut) {
    return(formatC(p, format = "e", digits = 2))
  }

  round_p <- round(p, digits)

  formatC(round_p, format = "f", digits = digits)
}


#' @keywords internal
.cramers_v <- function(tab) {

  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))

  n <- sum(tab)

  r <- nrow(tab)
  c <- ncol(tab)

  v <- sqrt(
    as.numeric(chi$statistic) /
      (n * min(r - 1, c - 1))
  )

  v
}


#' @keywords internal
.odds_ratio_ci <- function(tab, conf = 0.95) {

  if (!all(dim(tab) == c(2,2)))
    stop("Odds ratio only for 2x2 tables.")

  a <- tab[1,1]
  b <- tab[1,2]
  c <- tab[2,1]
  d <- tab[2,2]

  # Haldane correction
  if (any(tab == 0)) {
    a <- a + 0.5
    b <- b + 0.5
    c <- c + 0.5
    d <- d + 0.5
  }

  or <- (a * d) / (b * c)

  se <- sqrt(1/a + 1/b + 1/c + 1/d)

  z <- qnorm(1 - (1 - conf)/2)

  low <- exp(log(or) - z * se)
  high <- exp(log(or) + z * se)

  list(
    or = or,
    ci_low = low,
    ci_high = high
  )
}
