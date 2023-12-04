#' Title
#'
#' @param data
#' @param splits_each
#' @param mod_names
#' @param cols_each
#'
#' @return
#'
#' @examples
#'
#' @export
split_data <- function(
    data, splits_each, cols_each = NULL, mod_names = NULL) {
  # Setup:
  n <- nrow(data)
  k <- length(splits_each)
  splits_each <- purrr::map(splits_each, ~ c(.x, n)[1:2])
  cols_each <- cols_each %||% rep(list(colnames(data)), k)
  mod_names <- mod_names %||% paste0("model", 1:k)

  if (length(cols_each) != k) {
    abort("`cols_each` must have `length(splits_each)` elements")
  }
  if (!all(purrr::map_lgl(splits_each, ~ all(.x %in% 1:n)))) {
    abort("All elements of `splits_each` must be within `1:nrow(data)`")
  }

  result <- purrr::map2(splits_each, cols_each, function(splits, cols) {
    dplyr::select(data[splits[1]:splits[2],], dplyr::all_of(cols))
  }) %>%
    purrr::set_names(mod_names)

  result
}


#' Title
#'
#' @param vars
#' @param k
#' @param rmv_each
#' @param mnt_each
#'
#' @return
#' @export
#'
#' @examples
split_cols <- function(vars, k, rmv_each, mnt_each, exact = FALSE) {
  exatify <- function(x, exact) if (exact) paste0("$", x, "^") else x

  rmv_each <- if (length(rmv_each) == 1) rep(rmv_each, k) else rmv_each
  mnt_each <- if (length(mnt_each) == 1) rep(mnt_each, k) else mnt_each

  purrr::map2(rmv_each, mnt_each, function(rmv, mnt) {
    rmv_ind <- grepl(gsub("^$", "$^", rmv) %>% exatify(exact), vars)
    mnt_ind <- grepl(mnt %>% exatify(exact), vars)
    vars[mnt_ind & !rmv_ind]
  })
}
