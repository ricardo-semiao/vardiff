#' Title
#'
#' @param data
#' @param exogen_fun
#' @param exogen_list
#' @param k
#'
#' @return
#' @export
#'
#' @examples
map_exogen <- function(data, exogen_fun, exogen_list, k) {
  if (is.null(exogen_fun)) {
    exogen_list <- rep(list(NULL), k)
  } else {
    exogen_args <- exogen_args %||% rep(list(), k)
    exogen_list <- purrr::map2(data, exogen_args, ~ inject(exogen_fun(.x, !!!.y)))
  }
}
