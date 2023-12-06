#' @noRd
setup_split_data <- function(
    data, splits_each, cols_each, splits_names) {
  # Tests:
  test$class_arg(data, c("data.frame", "matrix"))
  test$type_arg(splits_each, "list")
  test$type_arg(cols_each, "list", length(splits_each))
  test$type_arg(splits_names, "character", length(splits_each))

  test$cols_each(cols_each, names(data))
  test$splits_each(splits_each, nrow(data))

  # Reassign:
  k <- length(splits_each)

  list(
    splits_each = purrr::map(splits_each, ~ c(.x, nrow(data))[1:2]),
    cols_each = cols_each %||% rep(list(colnames(data)), k),
    splits_names = splits_names %||% paste0("model", 1:k)
  )
}


#' @noRd
setup_split_cols <- function(
    cols_names, k, mnt_each, rmv_each, prio, drop, exact) {
  # Tests:
  test$type_arg(cols_names, "character")
  test$type_arg(k, "numeric", 1)
  if (!rlang::is_string(mnt_each)) test$type_arg(mnt_each, "list", 1)
  if (!rlang::is_string(rmv_each)) test$type_arg(rmv_each, "list", 1)
  test$categorical_arg(prio, c("mnt", "rmv"))
  test$type_arg(drop, "boolean")
  test$type_arg(exact, "boolean")

  if (!length(mnt_each) %in% c(1, k) || !length(rmv_each) %in% c(1, k)) {
    cli_abort("`mnt_each` and `rmv_each` must have length `k` ({k}).",
              call = caller_env()
    )
  }

  # Reassign:
  list(
    k = max(length(mnt_each), length(rmv_each)),
    exactify = function(x) {
      if (exact) paste0("^", x, "$") else x
    }
  )
}


#' Split data for modelling
#'
#' Split a dataset in custom points to later create a VAR model with each split.
#'
#' @param data A dataset (object coercible to data.frame) to split.
#' @eval param_splits()
#' @eval param_cols()
#' @param splits_names Names for each split, default to "model1", "model2", ...
#'
#' @return A list with each data split (length = \code{length(splits_each)}).
#'
#' @examples
#' splits_each <- list(c(1, 620), c(621, 1240), c(1241))
#' split_data(EuStockMarkets, splits_each)
#'
#' @export
split_data <- function(
    data, splits_each, cols_each = NULL, splits_names = NULL) {
  # Setup:
  setup <- setup_split_data(data, splits_each, cols_each, splits_names)
  reassign <- c("splits_each", "cols_each", "splits_names")
  list2env(setup[reassign], envir = current_env())

  data <- tibble::as_tibble(data)

  # Operations:
  result <- purrr::map2(splits_each, cols_each, function(splits, cols) {
    dplyr::select(data[splits[1]:splits[2],], dplyr::all_of(cols))
  }) %>%
    purrr::set_names(splits_names)

  # Result:
  result
}


#' Define columns to use in each model
#'
#' Create a list of columns to used in each VAR model, using REGEX or exact
#'  matching. The logic works as below:
#' * \code{mnt_each} and \code{rmv_each} are to be lists of \code{k} character
#'  vectors, each with matches to look for maintaining or removing columns
#'  (respectively).
#' * \code{prio} defines if only columns not matched by \code{mnt_each} should
#'  be considered for \code{rmv_each} (the default), or the other way around.
#' * \code{drop} defines what should be done with unmatched cols, kept (the
#'  default), or dropped.
#'
#' @param cols_names A character vector of column names in your data.
#' @param mnt_each
#' @param rmv_each
#' @param k The number of splits being used. Defaults to the max length between
#'  \code{mnt_each} and \code{rmv_each}, but pass it manually if using theese
#'  arguments in their unit length versions.
#' @param prio Which of \code{mnt_each} (\code{"mnt"}, the default) and
#'  \code{rmv_each} (\code{"rmv"}) should take precedence in the matching.
#' @param drop Should unmatched columns be dropped? The default is \code{FALSE}
#'  for keeping the columns.
#' @param exact Should \code{mnt_each} and \code{rmv_each} be interpreted as
#'  literal strings? The default is \code{FALSE} for REGEX matching.
#'
#' @return A list with each column vector (length = \code{k}).
#'
#' @examples
#' split_cols(c("DAX", "SMI", "CAC", "FTSE"), list(c("CAC", "DAX")), "SMI")
#'
#' @export
split_cols <- function(
    cols_names, mnt_each = ".", rmv_each = "", k = NULL,
    prio = "mnt", drop = FALSE, exact = FALSE) {
  # Setup:
  setup <- setup_split_cols(cols_names, k, rmv_each, mnt_each, prio, drop, exact)
  reassign <- c("k")
  list2env(setup[reassign], envir = current_env())

  ord <- if (prio == "mnt") 1:2 else 2:1

  rep_matches <- function(x) {
    if (rlang::is_string(x)) x <- list(x)
    if (length(x) == 1) {
      paste(setup$exactify(unlist(x)), collapse = "|") %>% list() %>% rep(k)
    } else {
      purrr::map(x, setup$exactify)
    }
  }

  # Operations:
  mnt_each <- rep_matches(mnt_each)
  rmv_each <- rep_matches(rmv_each)

  result <- purrr::map2(rmv_each, mnt_each, function(rmv, mnt) {
    mnt_ind <- grepl(mnt, cols_names)
    rmv_ind <- grepl(gsub("^$", "$^", rmv), cols_names)

    ind <- purrr::pmap_lgl(list(mnt_ind, !rmv_ind)[ord],
      if (drop) `&&` else `||`
    )
    cols_names[ind]
  })

  # Result:
  result
}
