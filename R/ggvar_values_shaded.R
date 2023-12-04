#' Title
#'
#' @param data
#' @param splits_each
#' @param cols
#' @param index
#' @param ...
#' @param line_args
#' @param facet_args
#' @param splits_names
#' @param colors
#'
#' @return
#' @export
#'
#' @examples
ggvar_values_shaded <- function(
    data, splits_each, index, cols = NULL, splits_names = NULL,
    ..., colors = NULL, line_args = list(), facet_args = list()) {
  k <- length(splits_each)
  n <- nrow(data)
  data <- tibble::as_tibble(data)
  index <- data[[index]]
  splits_names <- splits_names %||% seq_along(splits_each)

  args <- list(...)
  alpha <- if (is.null(args$alpha)) 0.5 else args$alpha

  colors <- varutils:::get_pallete(colors, k)

  splits <- purrr::map2_dfr(splits_each, splits_names,
    ~ c(c(.x, n)[1:2], .y) %>% purrr::set_names(c("xmin", "xmax", "fill"))
  ) %>%
    dplyr::mutate(fill = as.factor(fill))

  rects <- inject(ggplot2::geom_rect(
    aes(NULL, NULL,
        ymin = -Inf, ymax = Inf, xmin = xmin, xmax = xmax, fill = fill
    ),
    data = splits, alpha = alpha, !!!args[names(args) != "alpha"]
  ))

  varutils::ggvar_values(data, cols, index, line_args, facet_args) +
    rects +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(fill = "Splits")
}
