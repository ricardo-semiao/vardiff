#' Title
#'
#' @param data_list
#' @param p_list
#' @param exogen_fun
#' @param exogen_args
#' @param ignore_cols
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
map_var <- function(
    data_list, p_list,
    exogen_fun = NULL, exogen_args = NULL,
    cols_list = NULL, ...) {
  # Setup:
  k <- length(data_list)
  cols_list <- if (is.null(cols_list)) {
    purrr::map(data_list, colnames)
  } else {
    rep_args(k, cols_list)
  }

  var_args <- rep_args(k, list(...))
  exogen_list <- map_exogen(data, exogen_fun, exogen_args, k)

  purrr::pmap(list(data_list, p_list, exogen_list, var_args, cols_list),
    function(data, p, exogen, args, cols) {
      data <- dplyr::select(data, dplyr::all_of(cols))
      inject(vars::VAR(data, p, exogen = exogen, !!!args))
  })
}
