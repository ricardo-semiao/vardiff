#' @noRd
setup_mapvar_acf <- function(
    x_each, cols_each, plot, dots) {
  # Tests:
  test$type_arg(x_each, "list")
  purrr::walk(x_each, function(x_each_elements) {
    test$class_arg(x_each_elements, c("varest", "data.frame", "matrix"))
  })
  test$cols_each(cols_each, purrr::map(x_each, names))
  test$type_arg(plot, "boolean")
  test$fun_arg(dots, stats::acf)

  # Reassign:
  if (rlang::inherits_any(x_each, "varest")) {
    x_each <- tibble::as_tibble(stats::residuals(x_each))
  }

  list(
    x_each = x_each,
    cols_each = cols_each %||% purrr::map(x_each, colnames)
  )
}


#' @noRd
setup_mapvar_ccf <- function(
    x_each, cols, plot, patch, dots) {
  # Tests:
  test$type_arg(x_each, "list")
  purrr::walk(x_each, function(x_each_elements) {
    test$class_arg(x_each_elements, c("varest", "data.frame", "matrix"))
  })
  if (!is.null(cols)) test$type_arg(cols, "character")
  test$type_arg(plot, "boolean")
  test$type_arg(patch, "boolean")
  test$fun_arg(dots, varutils::ggvar_ccf_wrap)

  names_intersect <- purrr::reduce(purrr::map(x_each, names), intersect)
  if (!all(cols %in% names_intersect)) {
    cli_abort(
      "All elements of `cols` must be names in one of the splits/models",
      call = caller_env()
    )
  }

  # Reassign:
  if (rlang::inherits_any(x_each, "varest")) {
    x_each <- tibble::as_tibble(stats::residuals(x_each))
  }

  list(
    x_each = x_each,
    cols = cols %||% names_intersect
  )
}


#' Calculate ACF for each data split or model
#'
#' Calculate the Auto-Correlation function for each data split of each VAR  model
#'  residuals.
#'
#' @param x_each A list of datasets or VAR models to get variables or residuals
#'  from.
#' @eval param_cols()
#' @param plot If a ggplot should be returned (the default), instead of the raw
#'  data. Soon to be removed in favor of a \link[base]{plot} method.
#' @param ... Further arguments passed to \link[stats]{acf}.
#'
#' @return A list of datasets with the ACF information for each split/model, or
#'  a ggplot, if \code{plot = TRUE}
#'
#' @examples
#' data <- EuStockMarkets
#' data_each <- split_data(data, list(c(1, 620), c(621, 1240), c(1241)))
#' mapvar_acf(data_each, plot = FALSE)
#'
#' @export
mapvar_acf <- function(
    x_each, cols_each = NULL, plot = TRUE, ...) {
  # Setup:
  setup <- setup_mapvar_acf(x_each, cols_each, plot, list(...))
  reassign <- c("x_each", "cols_each")
  list2env(setup[reassign], envir = current_env())

  # Operations:
  data <- purrr::pmap(list(x_each, names(x_each), cols_each),
    function(x, model, cols) {
      dplyr::select(x, dplyr::all_of(cols)) %>%
        purrr::map2_dfr(names(.), function(col, name) {
          result_acf <- stats::acf(col, plot = FALSE, ...)
          tibble::tibble(
            serie = name,
            value = purrr::pluck(result_acf, "acf")[,,1],
            lag = purrr::pluck(result_acf, "lag")[,,1]
          )
        }) %>%
        dplyr::mutate(model = model)
    })

  # Results:
  if (plot) {
    ggplot(dplyr::bind_rows(data), aes(lag, value)) +
      ggplot2::geom_segment(aes(xend = lag, yend = 0)) +
      ggplot2::facet_grid(vars(serie), vars(model)) +
      ggplot2::labs(title = "ACF Difference", x = "Lag", y = "Value")
  } else {
    data
  }
}


#' Calculate CCF for each data split or model
#'
#' Calculate the Cross-Correlation function for each data split of each VAR
#'  model residuals.
#'
#' @param x_each A list of datasets or VAR models to get variables or residuals
#'  from.
#' @param cols Columns to consider.
#' @param plot If a ggplot should be returned (the default), instead of the raw
#'  data. Soon to be removed in favor of a \link[base]{plot} method.
#' @param patch If \code{plot = TRUE}, should the plots be wrapped
#'  (\link[patchwork]{wrap_plots}) together or not (the default)?
#' @param ... Further arguments passed to \link[stats]{acf}.
#'
#' @return A list of datasets with the ACF information for each split/model (if
#'  \code{plot = FALSE}), a list of ggplots (else if \code{patch = FALSE}), or a
#'  patched ggplot.
#'
#' @examples
#' data <- EuStockMarkets
#' data_each <- split_data(data, list(c(1, 620), c(621, 1240), c(1241)))
#' mapvar_ccf(data_each, plot = FALSE)
#'
#' @export
mapvar_ccf <- function(
    x_each, cols = NULL, plot = TRUE, patch = FALSE, ...) {
  # Setup:
  setup <- setup_mapvar_ccf(x_each, cols, plot, patch, list(...))
  reassign <- c("x_each", "cols")
  list2env(setup[reassign], envir = current_env())

  # Operations:
  result <- purrr::map(x_each, \(x) varutils::ggvar_ccf_wrap(x, cols, ...))

  # Results:
  if (plot) {
    if (patch) {
      patchwork::wrap_plots(result)
    } else {
      invisible(result)
    }
  } else {
    purrr::imap(result, ~ dplyr::mutate(.x$data, model = .y))
  }
}
