#' Title
#'
#' @param x_each
#' @param fun
#' @param ...
#' @param patch
#'
#' @return
#' @export
#'
#' @examples
vardiff_map <- function(
    x_each, fun, ..., patch = FALSE) {
  # Setup:
  k <- length(x_each)
  var_args <- rep_args(k, list(...))

  fun <- if (is.character(fun)) {
    switch(fun,
      "stability" = vars::stability,
      "arch" = vars::arch.test,
      "normality" = vars::normality.test,
      "serial" = vars::serial.test,
      "lik" = \(x) summary(x)$logLik,
      "roots" = \(x) summary(x)$roots,
      "covres" = \(x) summary(x)$covres,
      "corres" = \(x) summary(x)$corres,
      "obs" = \(x) summary(x)$obs,
      df = \(x) purrr::map_dbl(summary(x)$varresult, ~ .x$obs),
      r.squared = \(x) purrr::map_dbl(summary(x)$varresult, ~ .x$r.squared),
      adj.r.squared = \(x) purrr::map_dbl(summary(x)$varresult, ~ .x$adj.r.squared),
      fstatistic = \(x) purrr::map_dfr(summary(x)$varresult, ~ .x$fstatistic),
      cov.unscalred = \(x) purrr::map(summary(x)$varresult, ~ .x$cov.unscalred),
      get(paste0("ggvar_", fun), envir = rlang::ns_env("varutils"))
    )
  }

  result <- purrr::map2(x_each, var_args, \(x, args) inject(fun(x, !!!args)))

  if (patch) {
    patchwork::wrap_plots(result)
  } else {
    result
  }
}







