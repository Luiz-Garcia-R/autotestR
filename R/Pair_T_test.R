#' Paired t-test with advanced visualizations
#'
#' Performs a paired t-test between two numeric vectors (e.g., before vs after)
#' or between two numeric columns of a data frame.
#' Includes four visualization styles (boxplot, violin, monochrome, and half-eye).
#'
#' @param ... Two numeric vectors of equal length, or
#'   a data frame with exactly two numeric columns.
#' @param titulo Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param estilo Plot style:
#'   \itemize{
#'     \item \code{1} Premium boxplot
#'     \item \code{2} Violin + minimal boxplot
#'     \item \code{3} Monochrome
#'     \item \code{4} Half-eye (ggdist)
#'   }
#' @param conectar Logical. If TRUE, connects paired observations.
#' @param ajuda If TRUE, displays detailed help.
#' @param verbose If TRUE, prints progress messages.
#'
#' @return An invisible list containing:
#' \describe{
#'   \item{resumo}{Group means and standard deviations}
#'   \item{resultado}{t-test result object (stats::t.test)}
#'   \item{dados}{Data frame used for plotting}
#'   \item{plot}{ggplot2 object}
#' }
#'
#' @export
#'
#' @examples
#' before <- c(13, 12, 15, 14)
#' after  <- c(9, 11, 10, 10)
#' test.tpaired(before, after)
#'
#' df <- data.frame(A = before, B = after)
#' test.tpaired(df, estilo = 3)

test.tpaired <- function(
    ...,
    titulo = "Paired t-test",
    xlab = "",
    ylab = "Value",
    estilo = 1,
    conectar = TRUE,
    ajuda = FALSE,
    verbose = TRUE
) {

  args <- list(...)

  # ------------------------------
  # Help
  # ------------------------------
  if (ajuda) {
    message(
      "Function test.tpaired()

Accepted input:
 - Two numeric vectors of equal length
 - Or a data frame with exactly two numeric columns

# Example 1
before <- c(13, 12, 15, 14)
after  <- c(9, 11, 10, 10)
test.tpaired(before, after)

# Example 2
df <- data.frame(A = before, B = after)
test.tpaired(df, estilo = 3)

Returns:
 A list with summary, t.test result, plotting data and plot"
    )
    return(invisible(NULL))
  }

  # ------------------------------
  # Flexible input
  # ------------------------------
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) == 2) {
    df <- args[[1]]
    if (!all(sapply(df, is.numeric)))
      stop("The data frame must contain exactly two numeric columns.")
    x <- df[[1]]
    y <- df[[2]]
    nomes <- colnames(df)
  } else {
    if (length(args) != 2)
      stop("Provide two numeric vectors or a data frame with two columns.")

    x <- args[[1]]
    y <- args[[2]]

    if (!is.numeric(x) || !is.numeric(y))
      stop("Both vectors must be numeric.")

    if (length(x) != length(y))
      stop("Vectors must have the same length (paired test).")

    nomes <- as.character(match.call(expand.dots = FALSE)$...)[1:2]
  }

  # Remove missing pairs
  ok <- complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) < 3) stop("At least 3 valid paired observations are required.")

  # ------------------------------
  # Statistical results
  # ------------------------------
  resultado <- stats::t.test(x, y, paired = TRUE)
  p <- resultado$p.value

  p_label <- if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))
  signif_label <- if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""

  resumo <- data.frame(
    Group = nomes,
    Mean  = c(mean(x), mean(y)),
    SD    = c(sd(x), sd(y))
  )

  if (verbose) print(resumo)

  # ------------------------------
  # Final data frame
  # ------------------------------
  dados <- data.frame(
    id = seq_along(x),
    group = rep(nomes, each = length(x)),
    value = c(x, y)
  )

  y_pos <- max(dados$value) * 1.08

  # ------------------------------
  # Optional paired-line layer
  # ------------------------------
  camada_linhas <- function() {
    if (!conectar) return(NULL)
    ggplot2::geom_line(
      data = dados,
      ggplot2::aes(x = group, y = value, group = id),
      color = "gray40",
      linewidth = 0.5,
      alpha = 0.6
    )
  }

  cores_vivas <- scales::hue_pal()(length(unique(dados$group)))

  # ------------------------------
  # Plot styles
  # ------------------------------

  # ---- STYLE 1 ------------------------------------------------
  if (estilo == 1) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_boxplot(alpha = .7, outlier.shape = NA) +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .4
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Paired t-test: ", p_label),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # ---- STYLE 2 ------------------------------------------------
  if (estilo == 2) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_violin(trim = FALSE, alpha = .4, color = NA, adjust = .6) +
      ggplot2::geom_boxplot(width = .18, outlier.shape = NA, color = "gray20") +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .55, size = 1.8, color = "gray25"
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Paired t-test: ", p_label),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # ---- STYLE 3 ------------------------------------------------
  if (estilo == 3) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(group, value)) +
      ggplot2::geom_violin(fill = "gray85", color = NA) +
      ggplot2::geom_boxplot(width = .18, fill = "white") +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        color = "gray20", alpha = .4
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Paired t-test: ", p_label),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # ---- STYLE 4 (ggdist) ---------------------------------------
  if (estilo == 4) {
    if (!requireNamespace("ggdist", quietly = TRUE))
      stop("Package 'ggdist' is required for style = 4.")

    g <- ggplot2::ggplot(dados, ggplot2::aes(group, value, fill = group)) +
      ggdist::stat_halfeye(alpha = .6, adjust = .6) +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .4
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Paired t-test: ", p_label),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  print(g)

  invisible(list(
    resumo = resumo,
    resultado = resultado,
    dados = dados,
    plot = g
  ))
}
