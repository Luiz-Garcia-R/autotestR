#' ANOVA test with automated assumption checking
#'
#' Performs ANOVA (and Tukey HSD) if data meet normality and homogeneity assumptions.
#' Otherwise, automatically recommends Kruskal-Wallis/Dunn.
#'
#' @param ... Vectors or a data.frame with >= 2 columns.
#' @param title Plot title.
#' @param x X-axis label.
#' @param y Y-axis label.
#' @param style Aesthetic style of the generated plot.
#' @param help If TRUE, shows help.
#' @param verbose If TRUE, shows detailed messages.
#' @importFrom stats aov sd aggregate shapiro.test var.test
#' @return An `aov` object or a recommendation message.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   control = rnorm(30, 10),
#'   treatment = rnorm(30, 12),
#'   test = rnorm(30, 11)
#' )
#' test.anova(df)

test.anova <- function(..., title = "ANOVA/Tukey HSD", x = "X axis", y = "Y axis",
                       style = 1, help = FALSE, verbose = TRUE) {

  # --- Quick help block ---
  if (help || length(list(...)) == 0) {
    message("
test.anova() function

Description:
  Performs ANOVA between numeric groups, followed by Tukey HSD,
  if normality and homogeneity assumptions are met.
  Otherwise, recommends Kruskal-Wallis/Dunn.

Example:
  df <- data.frame(
    control = rnorm(30, 10, sd = 1),
    treatment = rnorm(30, 12, sd = 1),
    test = rnorm(30, 11, sd = 1)
  )

test.anova(df)
")
    return(invisible(NULL))
  }

  required_packages <- c("ggplot2", "multcompView", "car")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Please install package: ", pkg))
    }
  }

  dots <- list(...)
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    df <- dots[[1]]
    groups <- as.list(df)
    group_names <- names(df)
  } else {
    groups <- dots
    group_names <- sapply(substitute(list(...))[-1], deparse)
  }

  values <- unlist(groups)
  group_factor <- factor(rep(group_names, times = sapply(groups, length)))
  data_long <- data.frame(value = values, group = group_factor)

  apply_normality_test <- function(x) {
    if (length(x) < 3) NA else shapiro.test(x)$p.value
  }
  p_normal <- sapply(groups, apply_normality_test)
  normal <- all(p_normal > 0.05, na.rm = TRUE)

  p_levene <- tryCatch({
    if (length(groups) > 2) {
      car::leveneTest(value ~ group, data = data_long)$`Pr(>F)`[1]
    } else {
      var.test(groups[[1]], groups[[2]])$p.value
    }
  }, error = function(e) NA)
  homogeneous <- !is.na(p_levene) && p_levene > 0.05

  # -------------------------
  # If assumptions fail
  # -------------------------
  if (!normal || !homogeneous) {
    if (verbose) message("\nAssumptions failed. Recommendation: Kruskal-Wallis/Dunn")

    means_sd <- aggregate(value ~ group, data = data_long,
                          function(x) c(mean = mean(x), sd = sd(x)))
    means_sd <- do.call(data.frame, means_sd)
    colnames(means_sd)[2:3] <- c("mean", "sd")

    return(invisible(list(
      type = "ANOVA - assumptions not met",
      normal = normal,
      p_normal = p_normal,
      homogeneous = homogeneous,
      p_levene = p_levene,
      recommendation = "Use Kruskal-Wallis/Dunn",
      means_sd = means_sd
    )))
  }

  # -------------------------
  # ANOVA
  # -------------------------
  model <- aov(value ~ group, data = data_long)
  p_anova <- summary(model)[[1]][["Pr(>F)"]][1]

  # Automatic plot label
  p_label <- paste0("ANOVA: p = ", signif(p_anova, 3))

  # Tukey table
  tukey_res <- TukeyHSD(model)$group
  tukey_df <- as.data.frame(tukey_res)

  # Significant Tukey pairs (p < 0.05)
  tukey_pairs <- data.frame(
    Comparison = rownames(tukey_res),
    diff = tukey_res[, "diff"],
    lwr  = tukey_res[, "lwr"],
    upr  = tukey_res[, "upr"],
    p_adj = tukey_res[, "p adj"],
    stringsAsFactors = FALSE
  )

  significant_pairs <- subset(tukey_pairs, p_adj < 0.05)

  tukey_df$Comparison <- rownames(tukey_res)
  rownames(tukey_df) <- NULL

  # Significance letters
  letters <- multcompView::multcompLetters4(model, TukeyHSD(model))
  letters_df <- data.frame(
    group = names(letters$group$Letters),
    letter = letters$group$Letters,
    stringsAsFactors = FALSE
  )

  # Compute vertical position for each letter
  max_df <- aggregate(value ~ group, data = data_long, max)
  letters_df <- merge(letters_df, max_df, by = "group")
  letters_df$value <- letters_df$value + 0.2 * max(letters_df$value, na.rm = TRUE)

  # Means and SD
  means_sd <- aggregate(value ~ group, data = data_long,
                        function(x) c(mean = mean(x), sd = sd(x)))
  means_sd <- do.call(data.frame, means_sd)
  colnames(means_sd)[2:3] <- c("mean", "sd")

  # Friendly output
  if (verbose) {
    sep <- paste0(rep("=", 40), collapse = "")
    message("\nGroup summary (mean / sd)")
    message(sep)
    print(
      data.frame(
        Group = means_sd$group,
        Mean = round(means_sd$mean, 3),
        SD = round(means_sd$sd, 3)))
    message(sep)

    if (nrow(significant_pairs) == 0) {
      message("No significant post-hoc comparisons (Tukey, p < 0.05).")
    } else {
      message("\nSignificant pairs (Tukey HSD):")
      for (i in seq_len(nrow(significant_pairs))) {
        row <- significant_pairs[i, ]
        label_comp <- gsub("\\s*\\-\\s*", "-", row$Comparison)
        label_comp <- gsub("-", " - ", label_comp)
        message(sprintf("(%s, p = %s)",
                        label_comp,
                        signif(row$p_adj, 3)))
      }
    }
  }

  vivid_colors <- scales::hue_pal()(length(unique(data_long$group)))

  # --------------------------
  # Plot styles
  # --------------------------
  if (style == 1) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
      ggplot2::geom_text(data = letters_df,
                         ggplot2::aes(x = group, y = value, label = letter),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
  }

  if (style == 2) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA, adjust = 0.6) +
      ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
                            color = "gray20", linewidth = 0.4) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1),
                          alpha = 0.4, size = 1.8, color = "gray25") +
      ggplot2::geom_text(data = letters_df,
                         ggplot2::aes(x = group, y = value, label = letter),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
  }

  if (style == 3) {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(group, value)) +
      ggplot2::geom_violin(fill = "gray85", color = NA) +
      ggplot2::geom_boxplot(width = 0.18, fill = "white") +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1),
                          color = "gray20", alpha = 0.4) +
      ggplot2::geom_text(data = letters_df,
                         ggplot2::aes(x = group, y = value, label = letter),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
  }

  if (style == 4) {
    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("For style 4, please install the 'ggdist' package")
    }
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggdist::stat_halfeye(adjust = 0.6, width = 0.6,
                           .width = c(0.5, 0.8, 0.95),
                           justification = -0.2,
                           slab_color = "gray20",
                           interval_color = "gray20") +
      ggplot2::geom_point(position = ggplot2::position_nudge(x = 0.15),
                          size = 1.1, alpha = 0.4, color = "black") +
      ggdist::stat_pointinterval(position = ggplot2::position_nudge(x = 0.2),
                                 point_color = "black",
                                 interval_color = "black",
                                 .width = 0.95) +
      ggplot2::geom_text(data = letters_df,
                         ggplot2::aes(x = group, y = value, label = letter),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = title, subtitle = p_label, x = "", y = y) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
  }

  print(g)

  return(invisible(list(
    type = "ANOVA",
    p_anova = p_anova,
    normal = normal,
    p_normal = p_normal,
    homogeneous = homogeneous,
    p_levene = p_levene,
    means_sd = means_sd,
    tukey = tukey_df,
    significant_pairs = significant_pairs,
    letters = letters_df,
    model = model
  )))
}
