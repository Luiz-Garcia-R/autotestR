#' Multiple comparisons between numeric groups (t-test or Mann–Whitney),
#' with automatic diagnostics and annotated plot
#'
#' Performs multiple pairwise comparisons between numeric groups (e.g. columns
#' of a data frame or named vectors) using either Student's t-test or the
#' Mann–Whitney test (MW), automatically selected based on normality
#' (Shapiro–Wilk) and homogeneity of variances (Levene's test).
#'
#' The function also:
#' - accepts flexible input formats to define comparisons
#'   (list of pairs, character vectors, strings, etc.);
#' - prints descriptive summaries (mean / SD) and highlights significant pairs;
#' - generates annotated plots with significance bars and four visual styles;
#' - returns full results for programmatic use.
#'
#' @param ... A data frame containing only numeric columns, or multiple
#'   named numeric vectors (each representing a group).
#' @param comparisons Flexible format. Can be:
#'   * `NULL` (default), testing all pairwise comparisons;
#'   * `list(c("A","B"), c("C","D"))`;
#'   * character vector of even length, e.g. `c("A","B","C","D")`;
#'   * a single pair, e.g. `c("A","B")`;
#'   * a string such as `"A,B"` or `"A - B"`.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param style Integer 1–4 defining the plot style:
#'   1 = boxplot;
#'   2 = violin + boxplot;
#'   3 = minimalist violin;
#'   4 = half-eye (requires ggdist).
#' @param verbose Logical. If TRUE (default), prints messages and summaries.
#' @param help Logical. If TRUE, shows quick help.
#' @return An invisible list with:
#'   - `results`: tibble with group1, group2, method (t/MW) and p-value;
#'   - `significant_pairs`: subset with p < 0.05;
#'   - `plot`: final ggplot object;
#'   - `data_long`: data in long format.
#'
#' @examples
#' df <- data.frame(
#'   control   = rnorm(30, 10),
#'   treatment = rnorm(30, 12),
#'   test1     = rnorm(30, 11),
#'   test2     = rnorm(30, 15)
#' )
#'
#' test.tmulti(df)
#'
#' test.tmulti(
#'   df,
#'   comparisons = list(c("control", "treatment"),
#'                      c("treatment", "test1"))
#' )
#'
#' @export

test.tmulti <- function(...,
                         comparisons = NULL,
                         title = "Multiple comparisons (t / MW)",
                         xlab = "",
                         ylab = "Value",
                         style = 1,
                         help = FALSE,
                         verbose = TRUE) {

  # --- Quick help ---
  if (help || length(list(...)) == 0) {
    message("
Function test.tmulti()

Description:
  Performs multiple pairwise comparisons between numeric groups using
  Student's t-test or the Mann–Whitney test, automatically selected based
  on normality and homogeneity of variances. Generates annotated plots with
  significance bars.

Example:
  df <- data.frame(
    control   = rnorm(30, 10),
    treatment = rnorm(30, 12),
    test1     = rnorm(30, 11),
    test2     = rnorm(30, 15)
  )

  test.tmulti(df)

  test.tmulti(
    df,
    comparisons = list(c('control', 'treatment'),
                       c('treatment', 'test1'))
  )
")
    return(invisible(NULL))
  }

  # ------------------------------
  # Required packages
  # ------------------------------
  required_packages <- c("ggplot2", "purrr", "tibble", "tidyr",
                         "dplyr", "scales", "car")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is required to run test.tmulti().", pkg
      ), call. = FALSE)
    }
  }

  has_ggdist <- requireNamespace("ggdist", quietly = TRUE)

  # ------------------------------
  # Capture input (data frame or vectors)
  # ------------------------------
  dots <- list(...)
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    df <- as.data.frame(dots[[1]])
    group_order <- colnames(df)
  } else {
    group_values <- dots
    group_names <- sapply(substitute(list(...))[-1], deparse)
    df <- as.data.frame(group_values, stringsAsFactors = FALSE)
    names(df) <- group_names
    group_order <- group_names
  }

  if (!all(sapply(df, is.numeric))) {
    stop("All columns must be numeric.")
  }

  # ------------------------------
  # Normalize comparisons to list of pairs
  # ------------------------------
  to_pairs <- function(x) {

    if (is.list(x) &&
        all(sapply(x, function(z) is.character(z) && length(z) == 2))) {
      return(x)
    }

    if (is.character(x) && length(x) == 2) {
      return(list(x))
    }

    if (is.character(x) && length(x) == 1 && grepl("[-,;]", x)) {
      parts <- trimws(unlist(strsplit(x, "[-,;]+")))
      if (length(parts) == 2) return(list(parts))
    }

    if (is.character(x) && length(x) > 2) {
      if (length(x) %% 2 != 0) {
        stop("Character vector in 'comparisons' must have even length.")
      }
      return(split(x, rep(seq_along(x) / 2, each = 2)))
    }

    if (is.list(x) && all(sapply(x, is.character))) {
      flat <- unlist(x)
      if (length(flat) %% 2 != 0) {
        stop("List in 'comparisons' must contain an even number of elements.")
      }
      return(split(flat, rep(seq_along(flat) / 2, each = 2)))
    }

    stop("Unrecognized format for 'comparisons'.")
  }

  if (is.null(comparisons)) {
    cmb <- combn(group_order, 2)
    comparisons <- split(t(cmb), seq_len(ncol(cmb)))
  } else {
    comparisons <- to_pairs(comparisons)
  }

  # ------------------------------
  # Validate group names
  # ------------------------------
  all_names <- unique(unlist(comparisons))
  missing_names <- setdiff(all_names, names(df))
  if (length(missing_names) > 0) {
    stop(
      sprintf(
        "The following groups do not exist in the data: %s",
        paste(missing_names, collapse = ", ")
      )
    )
  }

  # ------------------------------
  # Build long-format data
  # ------------------------------
  data_long <- tibble::as_tibble(df) |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "group",
      values_to = "value"
    )

  data_long$group <- factor(data_long$group, levels = group_order)

  # ------------------------------
  # Test function for a pair
  # ------------------------------
  test_pair <- function(pair) {

    g1 <- pair[1]; g2 <- pair[2]
    v1 <- df[[g1]]; v2 <- df[[g2]]

    p_norm1 <- if (length(na.omit(v1)) >= 3)
      tryCatch(stats::shapiro.test(v1)$p.value, error = function(e) NA)
    else NA

    p_norm2 <- if (length(na.omit(v2)) >= 3)
      tryCatch(stats::shapiro.test(v2)$p.value, error = function(e) NA)
    else NA

    is_normal <- all(c(p_norm1, p_norm2) > 0.05, na.rm = TRUE)

    lev <- tryCatch(
      car::leveneTest(
        value ~ group,
        data = data_long[data_long$group %in% c(g1, g2), ]
      ),
      error = function(e) NA
    )

    is_homogeneous <- is.data.frame(lev) &&
      "Pr(>F)" %in% colnames(lev) &&
      lev$`Pr(>F)`[1] > 0.05

    if (is_normal && is_homogeneous) {
      res <- stats::t.test(v1, v2)
      method <- "t"
    } else {
      res <- stats::wilcox.test(v1, v2)
      method <- "MW"
    }

    tibble::tibble(
      group1 = g1,
      group2 = g2,
      method = method,
      p_value = as.numeric(res$p.value)
    )
  }

  # ------------------------------
  # Run tests
  # ------------------------------
  results <- purrr::map_dfr(comparisons, test_pair)

  # ------------------------------
  # Summary statistics
  # ------------------------------
  summary_stats <- data_long |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    )

  if (verbose) {
    message("\nGroup summary (mean / SD):")
    print(
      data.frame(
        Group = summary_stats$group,
        Mean = round(summary_stats$mean, 3),
        SD   = round(summary_stats$sd, 3)
      )
    )
  }

  # ------------------------------
  # Base plot
  # ------------------------------
  colors <- scales::hue_pal()(length(group_order))

  if (style == 1) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.12, alpha = 0.45, size = 1.6) +
      ggplot2::scale_fill_manual(values = colors)
  } else if (style == 2) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA) +
      ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA)
  } else if (style == 3) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(group, value)) +
      ggplot2::geom_violin(fill = "gray90", color = NA) +
      ggplot2::geom_boxplot(width = 0.16, fill = "white")
  } else if (style == 4 && has_ggdist) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(group, value, fill = group)) +
      ggdist::stat_halfeye(adjust = 0.6, width = 0.6)
  } else {
    stop("Invalid style. Use 1, 2, 3 or 4.")
  }

  g <- g +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme(legend.position = "none")

  if (verbose) print(g)

  invisible(list(
    results = results,
    significant_pairs = results[results$p_value < 0.05 & !is.na(results$p_value), ],
    plot = g,
    data_long = data_long
  ))
}
