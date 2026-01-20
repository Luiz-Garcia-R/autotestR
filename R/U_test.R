#' Teste de Mann-Whitney (U)
#'
#' Realiza o teste de Mann-Whitney (Wilcoxon rank-sum) para comparacao de dois grupos independentes,
#' com resumo estatistico e visualizacao grafica.
#'
#' @param ... Dois vetores numericos ou um data.frame com duas colunas numericas.
#' @param titulo Titulo do grafico. Default: "Teste de Mann-Whitney".
#' @param x Nome do eixo x no grafico. Default: "Grupo".
#' @param y Nome do eixo y no grafico. Default: "Valor".
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Se TRUE, imprime mensagens detalhadas. Default: TRUE.
#' @importFrom stats median
#'
#' @return Lista invisivel com:
#' \describe{
#'   \item{summary}{Resumo estatistico por grupo}
#'   \item{test}{Resultado do teste (objeto htest)}
#'   \item{plot}{Objeto ggplot2 com a visualizacao}
#' }
#' @export
#'
#' @examples
#' x <- c(1, 3, 5, 6)
#' y <- c(7, 8, 9, 12)
#' test.u(x, y)
#'
#' dados <- data.frame(grupoA = x, grupoB = y)
#' test.u(dados)

test.u <- function(...,
                    titulo = "Teste de Mann-Whitney",
                    x = "Grupo",
                    y = "Valor",
                    estilo = 1,
                    ajuda = FALSE,
                    verbose = TRUE) {

  input_groups <- list(...)

  # ============================
  # Entrada via data.frame
  # ============================
  if (length(input_groups) == 1 && is.data.frame(input_groups[[1]])) {

    df <- input_groups[[1]]

    if (ncol(df) != 2)
      stop("O data.frame deve conter exatamente duas colunas numericas.")

    if (!all(vapply(df, is.numeric, logical(1))))
      stop("As duas colunas do data.frame devem ser numericas.")

    group_names <- colnames(df)
    groups <- as.list(df)

  } else {

    if (length(input_groups) != 2)
      stop("Forneca dois vetores numericos ou um data.frame com duas colunas.")

    if (!all(vapply(input_groups, is.numeric, logical(1))))
      stop("Todos os grupos devem ser vetores numericos.")

    call_names <- as.character(match.call(expand.dots = FALSE)$...)
    group_names <- sub("^.*\\$", "", call_names)
    groups <- input_groups
  }

  # ============================
  # Mensagem de ajuda
  # ============================
  if (ajuda) {

    if (verbose) {
      message("
Funcao test.u()

Descricao:
  Realiza o teste de Mann-Whitney (Wilcoxon rank-sum) para comparar dois grupos independentes.

Quando usar:
  - Dados nao-normais ou ordinais
  - Comparacao entre dois grupos independentes
  - Alternativa nao-parametrica ao teste t

Exemplos:
  x <- c(1, 3, 5, 6)
  y <- c(7, 8, 9, 12)
  test.u(x, y)

  dados <- data.frame(grupoA = x, grupoB = y)
  test.u(dados)
")
    }

    return(invisible(NULL))
  }

  # ============================
  # Verificacao de pacotes
  # ============================
  required_packages <- c("ggplot2", "dplyr", "scales")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        paste0(
          "O pacote ", pkg,
          " nao esta instalado. Instale com install.packages('", pkg, "')"
        )
      )
    }
  }

  # ============================
  # Dados em formato longo
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
  # Teste Mann-Whitney
  # ============================
  test_result <- stats::wilcox.test(
    groups[[1]],
    groups[[2]],
    exact = FALSE
  )

  p_value <- test_result$p.value

  # ============================
  # Resumo estatistico
  # ============================
  summary_table <- data_long |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      Median = round(stats::median(value, na.rm = TRUE), 2),
      Mean = round(mean(value, na.rm = TRUE), 2),
      SD = round(stats::sd(value, na.rm = TRUE), 2),
      .groups = "drop"
    )

  if (verbose) {
    message("\nResumo estatistico por grupo:")
    print(summary_table)
  }

  # ============================
  # Rotulos de significancia
  # ============================
  p_label <- if (p_value < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", signif(p_value, 3))
  }

  signif_label <- if (p_value < 0.001) {
    "***"
  } else if (p_value < 0.01) {
    "**"
  } else if (p_value < 0.05) {
    "*"
  } else {
    ""
  }

  y_position <- max(values, na.rm = TRUE) +
    0.1 * diff(range(values, na.rm = TRUE))

  colors_vivid <- scales::hue_pal()(length(unique(data_long$group)))

  # ============================
  # ESTILO 1 — Boxplot + jitter
  # ============================
  if (estilo == 1) {

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value, fill = group)
    ) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.4, color = "black") +
      ggplot2::annotate(
        "text",
        x = mean(seq_along(group_names)),
        y = y_position,
        label = signif_label,
        size = 6
      ) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Mann-Whitney: ", p_label),
        x = "",
        y = y
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # ESTILO 2 — Violin clean
  # ============================
  if (estilo == 2) {

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
      ggplot2::annotate(
        "text",
        x = mean(seq_along(group_names)),
        y = y_position,
        label = signif_label,
        size = 7
      ) +
      ggplot2::scale_fill_manual(values = colors_vivid) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Mann-Whitney: ", p_label),
        x = "",
        y = y
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # ESTILO 3 — Monocromatico
  # ============================
  if (estilo == 3) {

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value)
    ) +
      ggplot2::geom_violin(fill = "gray85", color = NA) +
      ggplot2::geom_boxplot(width = 0.18, fill = "white") +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.1),
        color = "gray20",
        alpha = 0.4
      ) +
      ggplot2::annotate(
        "text",
        x = mean(seq_along(group_names)),
        y = y_position,
        label = signif_label,
        size = 7
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Mann-Whitney: ", p_label),
        x = "",
        y = y
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # ESTILO 4 — ggdist half-eye
  # ============================
  if (estilo == 4) {

    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("O pacote ggdist é necessario para o estilo 4.")
    }

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value, fill = group)
    ) +
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
      ggplot2::annotate(
        "text",
        x = mean(seq_along(group_names)),
        y = y_position,
        label = signif_label,
        size = 6,
        fontface = "bold"
      ) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = titulo,
        subtitle = paste0("Mann-Whitney: ", p_label),
        x = "",
        y = y
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  print(g)

  invisible(
    list(
      summary = summary_table,
      test = test_result,
      plot = g
    )
  )
}
