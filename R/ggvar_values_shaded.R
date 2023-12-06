setup_ggvar_values_shaded <- function(
    data, splits_each, index, cols, splits_names,
    colors, line_args, facet_args, dots) {
  # Tests:
  test$class_arg(data, c("data.frame", "matrix"))
  test$type_arg(splits_each, "list")
  test$type_arg(index, "character")
  test$type_arg(index, "character")
  test$type_arg(splits_names, "character", length(splits_each))
  test$fun_arg(line_args, ggplot2::geom_line)
  test$fun_arg(facet_args, ggplot2::facet_wrap)
  test$fun_arg(dots, ggplot2::geom_rect)

  test$splits_each(splits_each, nrow(data))

  # Reassign:
  args <- dots

  list(
    k = length(splits_each),
    n = nrow(data),
    data = tibble::as_tibble(data),
    index = if (is.null(index)) 1:nrow(data) else data[[index]],
    splits_names = splits_names %||% seq_along(splits_each),
    colors = varutils:::get_pallete(colors, length(splits_each)),
    args = args,
    alpha = if (is.null(args$alpha)) 0.5 else args$alpha
  )
}

#' Plot historic values with shaded splits
#'
#' Plots a faceted ggplot of historic values in \code{data}, but shades each
#'  split as defined by \code{splits_each}.
#'
#' @param data A dataset (object coercible to data.frame) to plot historic
#'  values from.
#' @eval param_splits()
#' @param ... Aditional arguments passed to \link[ggplot2]{geom_rect}.
#' @param index A column name to be used as the x axis. Defaults to a numeric \\
#'  sequence.
#' @param cols Variables to plot, passed to \link[varutils]{ggvar_values}.
#' @param splits_names Names for the shading legend. Defaults to a numeric \\
#'  sequence.
#' @eval param_colors()
#' @eval param_ggplot_args(c("geom_line", "facet_wrap"))
#'
#' @return A ggplot.
#'
#' @examples
#' data <- EuStockMarkets
#' splits_each <- list(c(1, 620), c(621, 1240), c(1241))
#' ggvar_values_shaded(data, splits_each, colors = "RColorBrewer::Dark2")
#'
#' @export
ggvar_values_shaded <- function(
    data, splits_each, ..., index = NULL, cols = NULL, splits_names = NULL,
    colors = NULL, line_args = list(), facet_args = list()) {
  # Setup:
  setup <- setup_ggvar_values_shaded(data, splits_each, index, cols,
    splits_names, colors, line_args, facet_args, list(...)
  )
  reassign <- c(
    "k", "n", "data", "index", "splits_names", "alpha", "colors", "args"
  )
  list2env(setup[reassign], envir = current_env())

  # Operations
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

  # Result:
  varutils::ggvar_values(data, cols, index, line_args, facet_args) +
    rects +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(fill = "Splits")
}
