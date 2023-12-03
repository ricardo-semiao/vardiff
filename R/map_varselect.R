#' Title
#'
#' @param data_list
#' @param lag_list
#' @param criteria
#' @param exogen_fun
#' @param exogen_args
#' @param star
#' @param star_args
#' @param ignore_cols
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
map_varselect <- function(
    data_list, lag_list, criteria = c("AIC", "HQ", "SC", "FPE"),
    exogen_fun = NULL, exogen_args = NULL,
    star = FALSE, star_args = list(),
    cols_list = NULL, ...) {
  # Setup:
  k <- length(data_list)
  criteria <- paste0(criteria, "(n)")
  cols_list <- if (is.null(cols_list)) {
    purrr::map(data_list, colnames)
  } else {
    rep_args(k, cols_list)
  }

  var_args <- rep_args(k, list(...))
  exogen_list <- map_exogen(data, exogen_fun, exogen_args, k)

  result <- purrr::pmap(list(data_list, lag_list, exogen_list, var_args, cols_list),
              function(data, lag, exogen, args, cols) {
                data <- dplyr::select(data, -dplyr::any_of(cols))
                inject(vars::VARselect(data, lag, exogen = exogen, !!!args)) %>%
                  purrr::map2(list(\(x)x[criteria], \(x)x[criteria,]), ~ .y(.x))
              })

  if (star) {
    purrr::transpose(result)$selection %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(Model = names(result), .before = 1) %>%
      {inject(stargazer::stargazer(., summary = FALSE, !!!star_args))}
    invisible(result)
  } else {
    result
  }
}
