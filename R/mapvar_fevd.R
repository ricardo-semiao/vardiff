mapvar_fevd <- function(mod_each, n.ahead = 10) {
  purrr:::map(mod_each, ~ vars::fevd(.x, n.ahead))
}

diff_fevd <- function(
    fevd_each, response = NULL, baseline = c(1, 1),
    plot = TRUE,
    bar_args = list(),
    facet_args = list()) {

  data <- purrr::imap(fevd_each, function(x, name) {
    x[[response]] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(horizon = 1:nrow(.), model = name)
  })

  result_graph <- ggplot(
    tidyr::pivot_longer(dplyr::bind_rows(data), -c("horizon", "model")),
    aes(horizon, value, fill = name)
  ) +
    inject(ggplot2::geom_bar(stat = "identity", !!!bar_args)) +
    inject(ggplot2::facet_wrap(vars(model), nrow = 1, !!!facet_args)) +
    ggplot2::labs(title = "IRF by Model", x = "Horizon", y = "Value")

  result_diff <- purrr::imap_dfc(data[-baseline[1]], function(x, name) {
    (data[[baseline[1]]][baseline[2]] - x[baseline[2]]) %>%
      purrr::set_names(name)
  })

  result_sum <- purrr::map_dbl(result_diff, sum)

  plot(result_graph)
  list(plot = result_graph, fevd_diff = result_diff, fevd_diff_sum = result_sum)
}
