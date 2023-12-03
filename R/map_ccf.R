ccf_diff <- function(data_list, x, y, ...) {
  data_ccf <- purrr::imap_dfr(data_list, function(data, name) {
    ci <- qnorm((1 - 0.95) / 2) / sqrt(nrow(data))
    temp_ccf <- ccf(data[[x]], data[[y]], plot = FALSE, ...)

    tibble::tibble(
      name = as.factor(name),
      lag = temp_ccf$lag[,,1],
      value = temp_ccf$acf[,,1],
      ci = ci
    )
  })

  ggplot(data_ccf, aes(x = lag, y = value)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_segment(aes(xend = lag, yend = 0)) +
    ggplot2::geom_ribbon(aes(ymin = -ci, ymax = ci), fill = NA) +
    ggplot2::facet_wrap(vars(name), ncol = 3) +
    ggplot2::labs(y = "Correlação", x = "Lag (casos)")
}


map_ccf <- function(
    data_list, xs = NULL, ys = NULL,
    lag.max = NULL, wrap_by = "y", plot = FALSE, ...) {
  xs <- xs %||% names(data_list[[1]])
  ys <- ys %||% names(data_list[[1]])

  combs <- expand.grid(x = xs, y = ys) %>% dplyr::filter(x != y)

  names <- if (wrap_by == "y") {
    combs$y
    } else if (wrap_by == "x") {
      combs$x
    } else if (wrap_by == "both") {
      rep("plot", nrow(combs))
    } else {
      formatC(1:nrow(combs), width = nchar(nrow(combs)), flag = "0")
    }

  combs_list <- combs %>% asplit(1) %>% {purrr::set_names(., names)}

  result <- purrr::map(combs_list, \(l) ccf_diff(data_list, l["x"], l["y"])) %>%
    split(names(.))#, ...

  if (plot) purrr::map(result, \(l) patchwork::wrap_plots(l))
  result
}



