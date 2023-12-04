#' Title
#'
#' @param mods
#' @param cause
#' @param ...
#' @param vcov_fun
#' @param vcov_args
#' @param star
#' @param star_args
#' @param star_names
#' @param star_round
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_causality <- function(
    mod_each, cause = NULL, ...,
    vcov_fun = NULL, vcov_args = list(),
    star = FALSE, star_args = list(), star_names = list(), star_round = 3) {
  mods_vcov <- if (is.null(vcov_fun)) {
    rep(list(NULL), length(mod_each))
  } else {
    purrr::map(mod_each, ~ inject(vcov_fun(.x, !!!vcov_args)))
  }
  k <- length(mod_each)
  causality_args <- rep_args(k, list(...))
  star_args <- rep_args(2, star_args)

  result_causality <- purrr::pmap(list(mod_each, mods_vcov, causality_args),
    function(mod, vcov, args) {
      inject(vars::causality(mod, cause = cause, vcov. = vcov, !!!args))
    }
  )

  if (star) {
    stats <- c("statistic", "parameter", "p.value")
    purrr::walk2(purrr::transpose(result_causality), star_args,
      function(test, args) {
        purrr::map(test, ~ round(purrr::reduce(.x[stats], c), star_round)) %>%
          purrr::reduce(rbind) %>%
          tibble::as_tibble(.name_repair = "minimal") %>%
          dplyr::rename(
            `P-value` = dplyr::last_col(),
            !!!purrr::map(star_names, dplyr::any_of)
          ) %>%
          {inject(stargazer::stargazer(., summary = FALSE, !!!args))}
      })
    invisible(result_causality)
  } else {
    result_causality
  }
}
