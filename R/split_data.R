#' Title
#'
#' @param data
#' @param split_at
#' @param gap
#' @param rmv_vars
#'
#' @return
#' @export
#'
#' @examples
split_data <- function(data, split_at, gap = NULL, rmv_vars = NULL) {
  # Setup:
  n <- nrow(data)
  rmv_vars <- rmv_vars %||% rep(list(""), 3)

  if (length(rmv_vars) < if (is.null(gap)) 2 else 3) {
    abort("`rmv_vars` must have 2 elements with `gap = NULL`, 3 otherwise")
  }
  if (!all(c(split_at, split_at + gap) %in% 1:n)) {
    abort("`split_at` and `split_at + gap` must be within `1:nrow(data)`")
  }

  # Operation:
  result <- list(
    before = data %>%
      dplyr::slice(1:(split_at - 1)) %>%
      dplyr::select(-dplyr::any_of(rmv_vars[[1]])),
    after = data %>%
      dplyr::slice(split_at:n) %>%
      dplyr::select(-dplyr::any_of(rmv_vars[[2]]))
  )

  if (!is.null(gap)) {
    result$after_gap <- data %>%
      dplyr::slice((split_at + gap):n) %>%
      dplyr::select(-dplyr::any_of(rmv_vars[[3]]))
  }

  result
}
