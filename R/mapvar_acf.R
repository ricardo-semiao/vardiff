#' Title
#'
#' @param data_each
#' @param cols_each
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_acf <- function(
    data_each, cols_each = NULL, plot = TRUE, ...) {
  cols_each <- cols_each %||% purrr::map(data_each, colnames)

  data <- purrr::pmap_dfr(list(data_each, names(data_each), cols_each),
    function(x, model, cols) {
      dplyr::select(x, dplyr::all_of(cols)) %>%
        purrr::map2_dfr(names(.), function(col, name) {
          result_acf <- stats::acf(col, plot = FALSE, ...)
          tibble::tibble(
            serie = name,
            value = purrr::pluck(result_acf, "acf")[,,1],
            lag = purrr::pluck(result_acf, "lag")[,,1]
          )
        }) %>%
        dplyr::mutate(model = model)
    })

  if (plot) {
    ggplot(data, aes(lag, value)) +
      ggplot2::geom_segment(aes(xend = lag, yend = 0)) +
      ggplot2::facet_grid(vars(serie), vars(model)) +
      ggplot2::labs(title = "ACF Difference", x = "Lag", y = "Value")
  } else {
    data
  }
}


#' Title
#'
#' @param data_each
#' @param cols
#' @param patch
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_ccf <- function(
    data_each, cols = NULL, plot = TRUE, patch = FALSE, ...) {
  cols <- cols %||% purrr::reduce(purrr::map(data_each, names), intersect)

  result <- purrr::map(data_each, \(x) varutils::ggvar_ccf_wrap(x, cols, ...))

  if (plot) {
    if (patch) {
      patchwork::wrap_plots(result)
    } else {
      invisible(result)
    }
  } else {
    purrr::imap_dfr(result, ~ dplyr::mutate(.x$data, model = .y))
  }
}
