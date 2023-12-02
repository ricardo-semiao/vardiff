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
    ignore_cols = "", ...) {
  # Setup:
  k <- length(data_list)

  var_args <- rep_args(k, list(...))

  exogen_list <- map_exogen(data, exogen_fun, exogen_args, k)

  purrr::pmap(list(data_list, p_list, exogen_list, var_args),
    function(data, p, exogen, args) {
      data <- dplyr::select(data, -dplyr::any_of(ignore_cols))
      inject(vars::VAR(data, p, exogen = exogen, !!!args))
  })
}
