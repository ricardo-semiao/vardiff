#' Create exogenous variables for VAR models
#'
#' This function creates a set of exogenous variables for each data split. It
#'  takes a custom function and a set of arguments for each split. This can be
#'  used to supply the \code{exogen} argument of functions in the "vars"
#'  package.
#'
#' @param data_each A list of data splits to build exogenous variables with
#'  each.
#' @param fun A function that takes a data split, and a set of additional
#'  arguments, and returns a dataset with exogenous variables.
#' @param args_each A list with sets of exogenous arguments to be passed to
#'  \code{fun} for each split. It can be a single item or a list with unit
#'  length to pass the same arguments for each split.
#' @param k The number of splits. Should be \code{length(data_each)}.
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_exogen <- function(
    data_each, fun = NULL, args_each = NULL, k = length(data_each)) {
  if (is.null(fun)) {
    rep(list(NULL), k)
  } else {
    if (is.null(args_each)) {
      args_each <- rep(list(), k)
    } else if (length(args_each) == 1) {
      args_each <- rep(args_each, k)
    }
    purrr::map2(data_each, args_each, ~ inject(fun(.x, !!!.y)))
  }
}
