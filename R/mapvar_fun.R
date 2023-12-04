#' @noRd
get_fun <- function(what, cols = NULL) {
  cols <- cols %||% purrr::reduce(purrr::map(data_each, names), intersect)
  get_summary <- function(x) summary(x)$varresult %>% .[names(.) %in% cols]

  if (is.function(what)) return(what)

  summaries <- c(
    "lik", "roots", "covres", "corres", "obs", "r.squared", "adj.r.squared",
    "fstatistic", "cov.unscalred"
  )
  tests <- c("stability", "arch", "normality", "serial")
  varutils <- c(
    "acf", "ccf_grid", "ccf_ind", "ccf_wrap", "dispersion", "distribution",
    "fevd", "fit", "fit_colored", "irf", "predict", "select", "stability",
    "values", "values_colored"
  )
  opts <- c(summaries, tests, varutils)

  if (!what %in% opts) abort(c(
    "Invalid `what`. Use one of:",
    "*" = "Summary elements to get: {summaries}",
    "*" = "Tests to apply: {tests}",
    "*" = "Varutils functions to plot: {varutils}"
  ))

  switch(what,
         "lik" = \(x) summary(x)$logLik,
         "roots" = \(x) summary(x)$roots,
         "covres" = \(x) summary(x)$covres,
         "corres" = \(x) summary(x)$corres,
         "obs" = \(x) summary(x)$obs,
         "df" = \(x) purrr::map_dbl(get_summary(x), ~ .x$obs),
         "r.squared" = \(x) purrr::map_dbl(get_summary(x), ~ .x$r.squared),
         "adj.r.squared" = \(x) purrr::map_dbl(get_summary(x), ~ .x$adj.r.squared),
         "fstatistic" = \(x) purrr::map_dfr(get_summary(x), ~ .x$fstatistic),
         "cov.unscalred" = \(x) purrr::map(get_summary(x), ~ .x$cov.unscalred),
         "stability" = vars::stability,
         "arch" = vars::arch.test,
         "normality" = vars::normality.test,
         "serial" = vars::serial.test,
         get(paste0("ggvar_", what), envir = rlang::ns_env("varutils"))
  )
}


#' Title
#'
#' @param x_each
#' @param what
#' @param ...
#' @param pretty
#' @param pretty_cond
#' @param cols
#' @param patch_args
#' @param broom_args
#' @param star_args
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_fun <- function(
    x_each, what, ...,
    pretty = "none", pretty_cond = NULL, cols = NULL,
    patch_args = list(), broom_args = list(), star_args = list()) {
  k <- length(x_each)
  fun_args <- rep_args(k, list(...))
  fun <- get_fun(what, cols)

  result <- purrr::map2(x_each, fun_args, \(x, args) inject(fun(x, !!!args)))

  if (pretty == "none") {
    result
  } else if (pretty == "patch") {
    inject(patchwork::wrap_plots(result, !!!patch_args))
  } else {
    pretty_cond <- pretty_cond %||% \(x) inherits(x, "htest")
    pretty_fun <- inject(generics::tidy(x, !!!broom_args))
    if (pretty == "stargazer") {
      pretty_fun <- purrr::compose(
        pretty_fun, \(x) inject(stargazer::stargazer(x, !!!star_args))
      )
    }
    search_map(result, pretty_cond, pretty_fun)
  }
}
