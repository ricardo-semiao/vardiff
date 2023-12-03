#' Title
#'
#' @param data
#' @param splits
#' @param cols_each
#' @param names
#'
#' @return
#' @export
#'
#' @examples
split_data <- function(
    data, split_at, gap = NULL,
    cols_list = NULL, names = NULL) {
  # Setup:
  n <- nrow(data)
  cols_list <- if (is.null(cols_list)) {
    rep(list(colnames(data)), 3)
  } else {
    cols_list
  }

  if (length(cols_list) < if (is.null(gap)) 2 else 3) {
    abort("`cols_list` must have 2 elements with `gap = NULL`, 3 otherwise")
  }
  if (!all(c(split_at, split_at + gap) %in% 1:n)) {
    abort("`split_at` and `split_at + gap` must be within `1:nrow(data)`")
  }

  # Operation:
  result <- list(
    before = data %>%
      dplyr::slice(1:(split_at - 1)) %>%
      dplyr::select(dplyr::all_of(cols_list[[1]])),
    after = data %>%
      dplyr::slice(split_at:n) %>%
      dplyr::select(dplyr::all_of(cols_list[[2]])),
    after_gap = if (!is.null(gap)) {
      data %>%
        dplyr::slice((split_at + gap):n) %>%
        dplyr::select(dplyr::all_of(cols_list[[3]]))
    }
  ) %>%
    purrr::set_names(names %||% names(.))

  result
}


#' Title
#'
#' @param vars
#' @param k
#' @param rmv_list
#' @param mnt_list
#'
#' @return
#' @export
#'
#' @examples
split_cols <- function(vars, k, rmv_list, mnt_list) {
  rmv_list <- if (length(rmv_list) == 1) rep(rmv_list, k) else rmv_list
  mnt_list <- if (length(mnt_list) == 1) rep(mnt_list, k) else mnt_list

  purrr::map2(rmv_list, mnt_list, function(rmv, mnt) {
    rmv_ind <- grepl(gsub("^$", "$^", rmv), vars)
    mnt_ind <- grepl(mnt, vars)
    vars[mnt_ind & !rmv_ind]
  })
}
