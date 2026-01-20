#' Kruskal-Wallis Test with Dunn Post-hoc
#'
#' Performs the Kruskal-Wallis test for comparing multiple independent groups,
#' followed by Dunn's post-hoc test with Bonferroni correction.
#'
#' @param ... Numeric vectors or a data frame with two or more columns (each representing a group)
#' @param title Plot title (default = "Kruskal-Wallis + Dunn")
#' @param x_label X-axis label (default = "Group")
#' @param y_label Y-axis label (default = "Value")
#' @param style Plot style.
#' @param help Logical. If TRUE, shows help message
#' @param verbose Logical. If TRUE, prints detailed messages (default = TRUE)
#'
#' @return Kruskal-Wallis test object (invisible)
#' @export

test.kruskal <- function(...,
                              title = "Kruskal-Wallis + Dunn",
                              x_label = "Group",
                              y_label = "Value",
                              style = 1,
                              help = FALSE,
                              verbose = TRUE) {

  # Capture arguments
  args <- list(...)

  # Allow data frame input
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) >= 2) {
    groups <- lapply(args[[1]], function(col) col)
    group_names <- colnames(args[[1]])
  } else {
    groups <- args
    raw_names <- as.character(match.call(expand.dots = FALSE)$...)
    group_names <- sub("^.*\\$", "", raw_names)
  }

  # Help message
  if (help || length(groups) < 2) {
    message("
Function test.kruskal()

Description:
  Kruskal-Wallis test (non-parametric ANOVA) followed by Dunn's post-hoc test.
  Ideal for comparing three or more independent groups with non-normal data.

Example:
  df <- data.frame(
    control = c(5, 6, 7),
    treatment1 = c(8, 9, 10),
    treatment2 = c(2, 3, 4)
  )
  test.kruskal(df)
")
    return(invisible(NULL))
  }

  # Required packages
  required_packages <- c("ggplot2", "FSA", "dplyr", "multcompView", "RColorBrewer")
  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is not installed.", pkg), call. = FALSE)
    }
  })

  # Data preparation
  data <- data.frame(
    value = unlist(groups),
    group = factor(
      rep(group_names, times = sapply(groups, length)),
      levels = group_names
    )
  )

  # Kruskal-Wallis test
  kruskal_res <- stats::kruskal.test(value ~ group, data = data)
  p_kruskal <- kruskal_res$p.value

  p_label <- ifelse(
    p_kruskal < 0.001,
    "Kruskal-Wallis: p < 0.001",
    paste0("Kruskal-Wallis: p = ", signif(p_kruskal, 3))
  )

  # Means and standard deviations (no automatic printing)
  mean_sd <- aggregate(
    value ~ group,
    data,
    function(x) c(mean = mean(x), sd = sd(x))
  )
  mean_sd <- do.call(data.frame, mean_sd)
  colnames(mean_sd)[2:3] <- c("mean", "sd")

  if (verbose) {
    sep <- paste0(rep("=", 50), collapse = "")
    message("Means and standard deviations by group")
    message(sep)
    print(mean_sd)
    message(sep)
  }

  # -----------------------------
  # Dunn post-hoc test
  # -----------------------------
  suppressMessages({
    suppressWarnings({
      dunn_res <- FSA::dunnTest(value ~ group, data = data, method = "bonferroni")
    })
  })

  dunn_df <- dunn_res$res
  significant_pairs <- subset(dunn_df, P.adj < 0.05)

  if (verbose) {
    if (nrow(significant_pairs) == 0) {
      message("No significant post-hoc comparisons (p < 0.05).")
    } else {
      msg <- paste0(
        "(",
        significant_pairs$Comparison,
        ", p = ",
        signif(significant_pairs$P.adj, 3),
        ")"
      )
      message("Significant pairs (Dunn, Bonferroni):")
      message(paste(msg, collapse = "\n"))
    }
  }

  # Significance letters
  comparisons <- setNames(
    dunn_df$P.adj,
    gsub(" ", "", dunn_df$Comparison)
  )

  letters_df <- multcompView::multcompLetters(comparisons)$Letters
  letters_df <- data.frame(
    group = names(letters_df),
    letter = unname(letters_df)
  )

  # Adjust letter positions
  max_values <- aggregate(value ~ group, data = data, max)
  letters_df <- merge(max_values, letters_df, by = "group")
  letters_df$value <- letters_df$value +
    0.2 * max(letters_df$value, na.rm = TRUE)

  # Labels and colors
  colors <- RColorBrewer::brewer.pal(
    length(unique(data$group)),
    "Set1"
  )

  # --------------------------
  # STYLE 1: Boxplot + jitter
  # --------------------------
  if (style == 1) {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
      ggplot2::geom_text(
        data = letters_df,
        ggplot2::aes(x = group, y = value, label = letter),
        size = 4,
        vjust = 0
      ) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y_label) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # STYLE 2: Violin + minimalist boxplot
  # --------------------------
  if (style == 2) {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
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
      ggplot2::geom_text(
        data = letters_df,
        ggplot2::aes(x = group, y = value, label = letter),
        size = 4,
        vjust = 0
      ) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y_label) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # STYLE 3: Monochrome premium
  # --------------------------
  if (style == 3) {
    g <- ggplot2::ggplot(data, ggplot2::aes(group, value)) +
      ggplot2::geom_violin(fill = "gray85", color = NA) +
      ggplot2::geom_boxplot(width = 0.18, fill = "white") +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.1),
        color = "gray20",
        alpha = 0.4
      ) +
      ggplot2::geom_text(
        data = letters_df,
        ggplot2::aes(x = group, y = value, label = letter),
        size = 4,
        vjust = 0
      ) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y_label) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # STYLE 4: Half-eye (ggdist)
  # --------------------------
  if (style == 4) {
    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("Style 4 requires the 'ggdist' package.")
    }

    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggdist::stat_halfeye(
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
      ggplot2::geom_text(
        data = letters_df,
        ggplot2::aes(x = group, y = value, label = letter),
        size = 4,
        vjust = 0
      ) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y_label) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  print(g)

  invisible(list(
    p_kruskal = p_kruskal,
    dunn = dunn_df,
    means = mean_sd
  ))
}
