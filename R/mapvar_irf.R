mapvar_irf <- function(mod_each, ..., unit = FALSE, mult = 1) {
  k <- length(mod_each)
  fun_args <- rep_args(k, list(...))

  result <- purrr:::map2(mod_each, fun_args,
    \(x, args) inject(vars::irf(x, !!!args))
  )

  if (unit) {
    purrr:::map2(result, mod_each, function(x, mod) {
      sds <- purrr::map_dbl(mod$datamat[1:mod$K], sd)
      x[1:3] <- purrr::map(x[1:3], \(col) purrr::map2(col, sds, ~ .x*mult/.y))
      x
    })
  } else {
    result
  }
}

diff_irf <- function(
    irf_each, response, impulse, baseline = 1,
    line_args = list(),
    ribbon_args = list(fill = NA, color = "blue", linetype = 2),
    hline_args = list(),
    facet_args = list()) {
  data <- purrr::imap(irf_each, function(x, name) {
    purrr::map_dfc(x[1:3], ~ .x[[response]][,impulse]) %>%
      dplyr::mutate(horizon = 1:nrow(.), model = name)
  })

  result_graph <- ggplot(dplyr::bind_rows(data),
    aes(horizon, irf, ymax = Upper, ymin = Lower)
    ) +
    inject(ggplot2::geom_line(!!!line_args)) +
    inject(ggplot2::geom_ribbon(!!!ribbon_args)) +
    inject(ggplot2::geom_hline(yintercept = 0, !!!hline_args)) +
    inject(ggplot2::facet_wrap(vars(model), nrow = 1, !!!facet_args)) +
    ggplot2::labs(title = "IRF by Model", x = "Horizon", y = "Value")

  cols <- c("irf", "Upper", "Lower")
  result_diff <- purrr::map(data[-baseline], ~ data[[baseline]][cols] - .x[cols])

  result_sum <- purrr::imap_dfr(result_diff, function(d, name) {
    purrr::map_dbl(d, ~ sum(.x)) %>% c(model = name, .)
  })

  plot(result_graph)
  list(plot = result_graph, irf_diff = result_diff, irf_diff_sum = result_sum)
}
