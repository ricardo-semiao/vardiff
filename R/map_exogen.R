#' Title
#'
#' @param data_each
#' @param exogen_fun
#' @param exogen_args
#' @param k
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_exogen <- function(data_each, fun, args, k) {
  if (is.null(fun)) {
    rep(list(NULL), k)
  } else {
    args <- args %||% rep(list(), k)
    purrr::map2(data_each, args, ~ inject(fun(.x, !!!.y)))
  }
}
