#' Correlation test (Pearson, Spearman or Kendall)
#'
#' Performs correlation testing between two numeric variables, automatically choosing
#' between Pearson, Spearman or Kendall based on normality, ties and outliers.
#' Optionally displays diagnostic plots and a correlation plot with trend line.
#'
#' @param x Numeric vector or data frame with two numeric columns
#' @param y Numeric vector (optional if x is a data frame)
#' @param method Correlation method: "auto" (default), "pearson", "spearman" or "kendall"
#' @param style Plot aesthetic style.
#' @param help If TRUE, displays a detailed explanation of the function
#' @param verbose If TRUE, prints messages about the chosen method and normality tests
#' @param plot_normality If TRUE, generates QQ-plots to assess data normality
#' @importFrom stats shapiro.test cor.test qqnorm qqline quantile
#'
#' @return Invisible object of class htest with the correlation test result
#'
#' @examples
#' # Pearson
#' set.seed(123)
#' x <- rnorm(50, sd = 0.1)
#' y <- x + rnorm(50, sd = 0.1)
#' test.correlation(x, y)
#'
#' # Spearman
#' set.seed(123)
#' x <- runif(300)
#' y <- log(x + 0.1) + rnorm(300, sd = 0.5)
#' test.correlation(x, y)
#'
#' # Kendall
#' set.seed(123)
#' x <- runif(1000, 1, 100)
#' y <- sin(x) * 30 + rnorm(1000, 0, 10)
#' x[sample(1:500, 50)] <- 50
#' y[sample(1:500, 50)] <- 0
#' x_out <- runif(100, 10, 20)
#' y_out <- runif(100, 80, 120)
#' x <- c(x, x_out)
#' y <- c(y, y_out)
#' test.correlation(x, y)
#'
#' @export

test.correlation <- function(x, y = NULL, method = "auto", style = 1, help = FALSE,
                             verbose = TRUE, plot_normality = FALSE) {

  if (help || missing(x)) {
    if (verbose) message("
test.correlation() function

Description:
  Tests correlation between two numeric variables, automatically choosing
  between Pearson, Spearman and Kendall.
  Can display graphical normality diagnostics (QQ-plots).

Usage:
  test.correlation(x, y, method = 'auto', plot_normality = TRUE)

Arguments:
  x, y              Numeric vectors of equal length or a data frame with two numeric columns
  method            'auto', 'pearson', 'spearman' or 'kendall'
  verbose           If TRUE, shows which method was used and why
  help              If TRUE, displays this help message with examples
  plot_normality    If TRUE, displays QQ-plots

Examples:

# Pearson
  set.seed(123)
  x <- rnorm(50, sd = 0.1)
  y <- x + rnorm(50, sd = 0.1)
  test.correlation(x, y)

# Spearman
  set.seed(123)
  x <- runif(300)
  y <- log(x + 0.1) + rnorm(300, sd = 0.5)
  test.correlation(x, y)

# Kendall
  set.seed(123)
  x <- runif(1000, 1, 100)
  y <- sin(x) * 30 + rnorm(1000, 0, 10)
  x[sample(1:500, 50)] <- 50
  y[sample(1:500, 50)] <- 0
  x_out <- runif(100, 10, 20)
  y_out <- runif(100, 80, 120)
  x <- c(x, x_out)
  y <- c(y, y_out)
  test.correlation(x, y)
")
    return(invisible(NULL))
  }

  # Required packages
  required_packages <- c("ggplot2", "ggExtra")
  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is not installed.", pkg), call. = FALSE)
    }
  })

  # Prepare data
  if (is.data.frame(x)) {
    if (ncol(x) != 2) stop("The data frame must contain exactly two numeric columns.")
    if (!all(sapply(x, is.numeric))) stop("Both data frame columns must be numeric.")
    name_x <- colnames(x)[1]
    name_y <- colnames(x)[2]
    data_df <- data.frame(x = x[[1]], y = x[[2]])
  } else {
    if (is.null(y)) stop("Provide two numeric vectors or a data frame with two columns.")
    if (!is.numeric(x) || !is.numeric(y)) stop("Both vectors must be numeric.")
    if (length(x) != length(y)) stop("Vectors must have the same length.")
    name_x <- deparse(substitute(x))
    name_y <- deparse(substitute(y))
    data_df <- data.frame(x = x, y = y)
  }

  # Checks and diagnostics
  has_ties <- anyDuplicated(data_df$x) > 0 || anyDuplicated(data_df$y) > 0

  apply_normality_test <- function(v) {
    n <- length(v)

    if (n <= 50) {
      p <- stats::shapiro.test(v)$p.value
      method <- "Shapiro-Wilk"

    } else if (n <= 300) {
      if (!requireNamespace("nortest", quietly = TRUE)) {
        stop("Please install the 'nortest' package for Anderson-Darling.")
      }
      p <- nortest::ad.test(v)$p.value
      method <- "Anderson-Darling"

    } else {
      ks_res <- stats::ks.test(v, "pnorm", mean = mean(v), sd = sd(v))
      p <- ks_res$p.value
      method <- "Kolmogorov-Smirnov"
    }

    list(p = p, method = method)
  }

  norm_x <- apply_normality_test(data_df$x)
  norm_y <- apply_normality_test(data_df$y)

  normal_x <- norm_x$p > 0.05
  normal_y <- norm_y$p > 0.05

  if (plot_normality) {
    oldpar <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(oldpar), add = TRUE)
    stats::qqnorm(data_df$x, main = paste("QQ-Plot of", name_x))
    stats::qqline(data_df$x, col = "red")
    stats::qqnorm(data_df$y, main = paste("QQ-Plot of", name_y))
    stats::qqline(data_df$y, col = "red")
  }

  detect_outliers <- function(v) {
    q1 <- stats::quantile(v, 0.25)
    q3 <- stats::quantile(v, 0.75)
    iqr <- q3 - q1
    which(v < (q1 - 1.5 * iqr) | v > (q3 + 1.5 * iqr))
  }

  outlier_fraction <- max(
    length(detect_outliers(data_df$x)) / nrow(data_df),
    length(detect_outliers(data_df$y)) / nrow(data_df)
  )

  method_used <- if (method == "auto") {
    if (outlier_fraction > 0.05) {
      "kendall"
    } else if (normal_x && normal_y && !has_ties) {
      "pearson"
    } else {
      "spearman"
    }
  } else {
    method
  }

  if (verbose && method == "auto") {
    message(sprintf("Selected method: %s", method_used))

    if (!normal_x || !normal_y) message("- Data are not normally distributed")
    if (has_ties) message("- Ties detected in the data")
    if (outlier_fraction > 0.05)
      message(sprintf("- Presence of outliers detected: %.1f%%",
                      outlier_fraction * 100))
  }

  # Statistical test
  test <- stats::cor.test(data_df$x, data_df$y, method = method_used)
  coef_value <- round(unname(test$estimate), 3)
  p_val <- test$p.value
  p_text <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", signif(p_val, 3))

  interpretation <- cut(
    abs(coef_value),
    breaks = c(-Inf, 0.3, 0.5, 0.7, 0.9, Inf),
    labels = c("very weak or none", "weak", "moderate", "strong", "very strong"),
    right = FALSE
  )

  symbol <- ifelse(method_used == "kendall", "tau", "r")
  subtitle <- paste0(symbol, " = ", coef_value, " | ", p_text, " | ", interpretation)

  # Choose smoothing method for trend line
  smooth_method <- if (tolower(method_used) == "pearson") "lm" else "loess"

  # Plot styles
  if (style == 1) {
    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.60, size = 2.5, col = "grey30") +
      ggplot2::geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(
        title = paste("Correlation:", tools::toTitleCase(method_used)),
        subtitle = subtitle,
        x = name_x,
        y = name_y
      )

  } else if (style == 2) {
    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.45, size = 2) +
      ggplot2::geom_density_2d(color = "grey30", alpha = 0.7) +
      ggplot2::geom_smooth(method = smooth_method, se = FALSE) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = paste("Correlation:", tools::toTitleCase(method_used)),
        subtitle = subtitle,
        x = name_x,
        y = name_y
      )

  } else if (style == 3) {
    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_hex() +
      ggplot2::scale_fill_gradientn(
        colours = c("#0000FF", "#00FF00", "#FFFF00", "#FF0000"),
        trans = "sqrt",
        name = "Density"
      ) +
      ggplot2::geom_smooth(method = smooth_method, se = FALSE, color = "white") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = paste("Correlation:", tools::toTitleCase(method_used)),
        subtitle = subtitle,
        x = name_x,
        y = name_y
      )

  } else if (style == 4) {

    if (!requireNamespace("ggExtra", quietly = TRUE)) {
      warning("For style = 4 install the 'ggExtra' package. Falling back to style 1.")
      g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.65, size = 2.5) +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = paste("Correlation:", tools::toTitleCase(method_used)),
          subtitle = subtitle,
          x = name_x,
          y = name_y
        )
    } else {
      base_plot <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.6, size = 2, col = "grey30") +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = paste("Correlation:", tools::toTitleCase(method_used)),
          subtitle = subtitle,
          x = name_x,
          y = name_y
        )

      g <- ggExtra::ggMarginal(base_plot, type = "density", size = 5, fill = "#c8d3e0")
    }

  } else {
    stop("Invalid style: use 1, 2, 3 or 4.")
  }

  print(g)
  invisible(test)
}
