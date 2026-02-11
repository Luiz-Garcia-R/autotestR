#' Mann-Whitney U Test
#'
#' Performs the Mann-Whitney (Wilcoxon rank-sum) test for comparing two independent groups,
#' with statistical summary and graphical visualization.
#'
#' @param ... Two numeric vectors or a data.frame with two numeric columns.
#' @param title Plot title. Default: "Mann-Whitney Test".
#' @param xlab Label for x-axis. Default: "Group".
#' @param ylab Label for y-axis. Default: "Value".
#' @param style Plot aesthetic style.
#' @param help Logical. If TRUE, prints a detailed explanation. Default: FALSE.
#' @param verbose Logical. If TRUE, prints detailed messages. Default: TRUE.
#' @importFrom stats median
#'
#' @return Invisible list with:
#' \describe{
#'   \item{summary}{Group-wise statistical summary}
#'   \item{test}{Test result (htest object)}
#'   \item{plot}{ggplot2 visualization object}
#' }
#' @export
#'
#' @examples
#' x <- c(1, 3, 5, 6)
#' y <- c(7, 8, 9, 12)
#' data <- data.frame(groupA = x, groupB = y)
#' test.u(data)

test.u <- function(...,
                   title = "Mann-Whitney Test",
                   xlab = "Group",
                   ylab = "Value",
                   style = c("boxplot", "violin", "monochrome", "halfeye"),
                   help = FALSE,
                   verbose = TRUE) {

  input_groups <- list(...)
  style <- match.arg(style)

  # ============================
  # Input via data.frame
  # ============================
  if (length(input_groups) == 1 && is.data.frame(input_groups[[1]])) {

    df <- input_groups[[1]]

    if (ncol(df) != 2)
      stop("The data.frame must contain exactly two numeric columns.")

    if (!all(vapply(df, is.numeric, logical(1))))
      stop("Both columns must be numeric.")

    group_names <- colnames(df)
    groups <- as.list(df)

  } else {

    if (length(input_groups) != 2)
      stop("Provide two numeric vectors or one data.frame with two columns.")

    if (!all(vapply(input_groups, is.numeric, logical(1))))
      stop("All groups must be numeric vectors.")

    call_names <- as.character(match.call(expand.dots = FALSE)$...)
    group_names <- sub("^.*\\$", "", call_names)
    groups <- input_groups
  }

  # ============================
  # Help message
  # ============================
  if (help) {

    if (verbose) {
      message("
Function: test.u()

Description:
  Performs the Mann-Whitney (Wilcoxon rank-sum) test to compare two independent groups.

When to use:
  - Non-normal or ordinal data
  - Comparison of two independent groups
  - Non-parametric alternative to the t-test

Examples:
  x <- c(1, 3, 5, 6)
  y <- c(7, 8, 9, 12)
  data <- data.frame(groupA = x, groupB = y)
  test.u(data)
")
    }

    return(invisible(NULL))
  }

  # ============================
  # Package checking
  # ============================
  required_packages <- c("ggplot2", "dplyr", "scales")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        paste0(
          "Package ", pkg,
          " is not installed. Install it with install.packages('", pkg, "')"
        )
      )
    }
  }

  # ============================
  # Long-format data
  # ============================
  values <- unlist(groups)

  group_factor <- factor(
    rep(group_names, times = vapply(groups, length, integer(1))),
    levels = group_names
  )

  data_long <- data.frame(
    value = values,
    group = group_factor
  )

  # ============================
  # Mann-Whitney test
  # ============================
  test_result <- stats::wilcox.test(
    groups[[1]],
    groups[[2]],
    exact = FALSE
  )

  p_value <- test_result$p.value
  p_label <- .format_pval(p_value)

  x_data <- groups[[1]]
  y_data <- groups[[2]]

  nx <- sum(!is.na(x_data))
  ny <- sum(!is.na(y_data))

  # ----------------------------
  # Median difference
  # ----------------------------
  median_diff <- median(x_data, na.rm = TRUE) - median(y_data, na.rm = TRUE)

  # ----------------------------
  # Rank-biserial correlation
  # ----------------------------
  U <- test_result$statistic

  r_rb <- as.numeric((2 * U) / (nx * ny) - 1)

  # ----------------------------
  # Bootstrap CI (median diff)
  # ----------------------------

  res_boot <- .boot_two_sample(
    x_data,
    y_data,
    stat_fun = function(a, b)
      median(a, na.rm = TRUE) - median(b, na.rm = TRUE)
  )

  ci_low  <- res_boot$ci_low
  ci_high <- res_boot$ci_high

  # ============================
  # Statistical summary
  # ============================
  summary_table <- data_long |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      Median = round(stats::median(value, na.rm = TRUE), 2),
      Mean   = round(mean(value, na.rm = TRUE), 2),
      SD     = round(stats::sd(value, na.rm = TRUE), 2),
      .groups = "drop"
    )

  if (verbose) {

    .print_header("Mann-Whitney U Test")

    .print_block("Summary", function() {
      print(summary_table, row.names = FALSE)
    })

    .print_block("Statistics", function() {

      cat("W statistic = ", round(test_result$statistic, 3),
          " | p = ", p_label, "\n", sep = "")

      cat("Rank-biserial correlation (r) = ",
          round(r_rb, 3), "\n", sep = "")

      cat("Median difference = ",
          round(median_diff, 2),
          " [",
          round(ci_low, 2), ", ",
          round(ci_high, 2),
          "]\n", sep = "")
    })
  }

  colors_vivid <- scales::hue_pal()(length(unique(data_long$group)))

  # ============================
  # STYLE 1 — Boxplot + jitter
  # ============================
  if (style == "boxplot") {

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value, fill = group)
    ) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.4, color = "black") +
      ggplot2::scale_fill_manual(values = colors_vivid) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # STYLE 2 — Violin
  # ============================
  if (style == "violin") {

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value, fill = group)
    ) +
      ggplot2::geom_violin(
        trim = FALSE,
        alpha = 0.55,
        color = NA,
        adjust = 0.6
      ) +
      ggplot2::geom_boxplot(
        width = 0.18,
        outlier.shape = NA,
        color = "gray20",
        linewidth = 0.4
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.1),
        alpha = 0.4,
        size = 1.8,
        color = "gray25"
      ) +
      ggplot2::scale_fill_manual(values = colors_vivid) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # STYLE 3 — Monochrome
  # ============================
  if (style == "monochrome") {

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value)
    ) +
      ggplot2::geom_violin(
        trim = FALSE,
        adjust = 0.6,
        fill = "gray85",
        color = NA
      ) +
      ggplot2::geom_boxplot(width = 0.18, fill = "white") +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.1),
        color = "gray20",
        alpha = 0.4
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # STYLE 4 — ggdist half-eye
  # ============================
  if (style == "halfeye") {

    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("Package 'ggdist' is required for style = 'halfeye'.")
    }

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value, fill = group)
    ) +
      ggdist::stat_halfeye(
        trim = FALSE,
        alpha = 0.6,
        adjust = 0.6,
        width = 0.6,
        .width = c(0.5, 0.8, 0.95),
        justification = -0.2,
        slab_color = "gray20",
        interval_color = "gray20"
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_nudge(x = 0.15),
        size = 1.1,
        alpha = 0.4,
        color = "black"
      ) +
      ggdist::stat_pointinterval(
        position = ggplot2::position_nudge(x = 0.2),
        point_color = "black",
        interval_color = "black",
        .width = 0.95
      ) +
      ggplot2::scale_fill_manual(values = colors_vivid) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  print(g)

  # ============================
  # Return
  # ============================
  invisible(
    list(
      summary = summary_table,
      test    = test_result,
      plot    = g
    )
  )
}
