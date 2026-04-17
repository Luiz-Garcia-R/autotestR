# ============================
# Auxiliary print functions
# ============================

#' @keywords internal
.print_header <- function(title) {
  cat("\n")
  cat(strrep("=", 50), "\n")
  cat(title, "\n")
  cat(strrep("=", 50), "\n")
}

#' @keywords internal
.print_block <- function(title, content, width = 40) {
  cat("\n", title, "\n", sep = "")
  cat(strrep("-", width), "\n")
  content()
  cat(strrep("-", width), "\n")
}

#' @keywords internal
.add_significance <- function(sig_pairs, y_range, text_size = 5) {

  if (is.null(sig_pairs) || nrow(sig_pairs) == 0) {
    return(NULL)
  }

  list(

    ggplot2::geom_segment(
      data = sig_pairs,
      ggplot2::aes(
        x = .data$x1,
        xend = .data$x2,
        y = .data$y,
        yend = .data$y
      ),
      inherit.aes = FALSE
    ),

    ggplot2::geom_segment(
      data = sig_pairs,
      ggplot2::aes(
        x = .data$x1,
        xend = .data$x1,
        y = .data$y,
        yend = .data$y - 0.02 * y_range
      ),
      inherit.aes = FALSE
    ),

    ggplot2::geom_segment(
      data = sig_pairs,
      ggplot2::aes(
        x = .data$x2,
        xend = .data$x2,
        y = .data$y,
        yend = .data$y - 0.02 * y_range
      ),
      inherit.aes = FALSE
    ),

    ggplot2::geom_text(
      data = sig_pairs,
      ggplot2::aes(
        x = (.data$x1 + .data$x2) / 2,
        y = .data$y + 0.02 * y_range,
        label = .data$signif
      ),
      inherit.aes = FALSE,
      size = text_size
    )
  )
}
