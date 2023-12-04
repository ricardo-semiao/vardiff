#' Title
#'
#' @param data
#' @param splits_each
#' @param cols
#' @param index
#' @param ...
#' @param line_args
#' @param facet_args
#'
#' @return
#' @export
#'
#' @examples
ggdiff_values <- function(
    data, splits_each, index, cols = NULL, mod_names = NULL,
    ..., line_args = list(), facet_args = list()) {
  k <- length(splits_each)
  n <- nrow(data)

  mod_names <- mod_names %||% paste0("model", 1:k)
  index <- data[[index]]
  splits_each <- purrr::map(splits_each, ~ c(.x, n)[1:2])

  rects <- purrr::map(splits_each, function(x) {
    ggplot2::annotate("rect", ...,
     ymin = -Inf, ymax = Inf, xmin = index[x[1]], xmax = index[x[2]]
    )
  })

  varutils::ggvar_values(data, cols, index, line_args, facet_args) +
    rects
}
