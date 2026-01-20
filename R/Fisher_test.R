#' Fisher's Exact Test
#'
#' Performs Fisher's Exact Test using two categorical vectors or a data frame with two columns,
#' constructing a contingency table and optionally generating graphical visualizations.
#'
#' @param x_var Categorical vector or data frame with two columns.
#' @param y_var Categorical vector (required if x_var is a vector).
#' @param title Plot title (string). Default: "Fisher's Exact Test"
#' @param x_label Name of the x-axis in the plot (string). Default: NULL (uses variable name)
#' @param y_label Name of the y-axis in the plot (string). Default: "Proportion"
#' @param show_table Logical. If TRUE, prints the contingency table to the console. Default: TRUE
#' @param style Plot style. Controls the visualization type.
#' @param help Logical. If TRUE, shows detailed function explanation. Default: FALSE
#' @param verbose Logical. If TRUE, prints detailed test messages. Default: TRUE
#'
#' @return Invisible object containing the Fisher test result.
#' @export
#'
#' @examples
#' data <- data.frame(control = c("healthy","healthy","sick","sick","sick"),
#'                    treatment = c("healthy","healthy","healthy","healthy","sick"))
#' test.fisher(data)

test.fisher <- function(x_var, y_var = NULL,
                             title = "Fisher's Exact Test",
                             x_label = NULL,
                             y_label = "Proportion",
                             style = 1,
                             show_table = TRUE,
                             help = FALSE,
                             verbose = TRUE) {

  # Help section
  if (help || missing(x_var)) {
    if (verbose) {
      message(
        "Function test.fisher()

Description:
  Performs Fisher's Exact Test to assess whether there is a significant association
  between two categorical variables.

When to use:
  - Two categorical variables (factors)
  - Small contingency tables (especially 2x2)
  - Expected frequencies below 5

Difference between Fisher and Chi-square:
  - Fisher: exact probability of observed combinations
  - Chi-square: theoretical approximation requiring larger samples"
      )
    }
    return(invisible(NULL))
  }

  # Required packages
  required_packages <- c("ggplot2", "dplyr", "tidyr", "vcd")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        paste0(
          "Package '", pkg,
          "' is not installed. Install it with install.packages('", pkg, "')"
        )
      )
    }
  }

  # Case: x_var is a data frame with two columns
  if (is.data.frame(x_var)) {
    if (ncol(x_var) != 2) {
      stop("The data frame must contain exactly two categorical columns.")
    }

    column_names <- colnames(x_var)

    data_long <- tidyr::pivot_longer(
      x_var,
      cols = everything(),
      names_to = "group",
      values_to = "category"
    )

    data_long$group <- factor(
      data_long$group,
      levels = column_names,
      labels = column_names
    )

    group_var <- data_long$group
    category_var <- data_long$category

    x_name <- column_names[1]
    y_name <- column_names[2]

  } else {
    # Case: x_var and y_var are vectors
    if (is.null(y_var)) {
      stop("y_var must be provided when x_var is a vector.")
    }
    if (length(x_var) != length(y_var)) {
      stop("The variables must have the same length.")
    }

    group_var <- x_var
    category_var <- y_var

    x_name <- deparse(substitute(x_var))
    y_name <- deparse(substitute(y_var))
    x_name <- sub(".*\\$", "", x_name)
    y_name <- sub(".*\\$", "", y_name)
  }

  if (is.null(x_label)) x_label <- x_name
  if (is.null(y_label)) y_label <- "Proportion"

  # Contingency table
  contingency_table <- table(group_var, category_var)

  if (verbose && show_table) {
    message("Observed contingency table:")
    print(contingency_table)
  }

  if (any(dim(contingency_table) > 2) && verbose) {
    message(
      "The table dimension is larger than 2x2. Fisher's test may be computationally intensive, ",
      "and the p-value is an approximation."
    )
  }

  # Fisher's Exact Test
  test_result <- stats::fisher.test(contingency_table)

  if (verbose) {
    message("Fisher's Exact Test")
    message("------------------")
    message("p-value: ", signif(test_result$p.value, 4))
  }

  # Data preparation for plotting
  plot_data <- data.frame(group = group_var, category = category_var)

  proportion_data <- plot_data |>
    dplyr::group_by(group, category) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(group) |>
    dplyr::mutate(proportion = n / sum(n))

  # --------------------------
  # STYLE 1 (Stacked bar plot)
  # --------------------------
  if (style == 1) {
    g <- ggplot2::ggplot(
      proportion_data,
      ggplot2::aes(x = group, y = proportion, fill = category)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = title,
        x = "",
        y = y_label,
        fill = y_name
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # STYLE 2 (Side-by-side bars)
  # --------------------------
  if (style == 2) {
    g <- ggplot2::ggplot(
      proportion_data,
      ggplot2::aes(x = group, y = proportion, fill = category)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_dodge(width = 0.8)
      ) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = title,
        x = "",
        y = y_label,
        fill = y_name
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # STYLE 3 (Mosaic plot)
  # --------------------------
  if (style == 3) {
    vcd::mosaic(
      contingency_table,
      shade = TRUE,
      legend = TRUE,
      main = title
    )
  }

  # --------------------------
  # STYLE 4 (Pie chart by group)
  # --------------------------
  if (style == 4) {
    g <- ggplot2::ggplot(
      proportion_data,
      ggplot2::aes(x = "", y = proportion, fill = category)
    ) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar("y") +
      ggplot2::facet_wrap(~ group) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = title,
        fill = y_name,
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          margin = ggplot2::margin(b = 25)
        )
      )
  }

  if (style != 3) print(g)
  invisible(test_result)
}
