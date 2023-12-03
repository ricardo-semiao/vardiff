map_acf <- function(
    data_list, cols_list = NULL, ...) {
  cols_list <- if (is.null(cols_list)) {
    purrr::map(data_list, colnames)
  } else {
    rep_args(k, cols_list)
  }

  data <- purrr::pmap_dfr(list(data_list, names(data_list), cols_list),
    function(x, model, cols) {
      dplyr::select(x, -dplyr::any_of(cols)) %>%
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

  ggplot(data, aes(lag, value)) +
    ggplot2::geom_segment(aes(xend = lag, yend = 0)) +
    ggplot2::facet_grid(vars(serie), vars(model)) +
    labs(title = "ACF Difference", x = "Lag", y = "Value")
}
