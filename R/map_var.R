#' Title
#'
#' @param data_each
#' @param p_each
#' @param cols_each
#' @param exogen_fun
#' @param exogen_args
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_var <- function(
    data_each, p_each, cols_each = NULL,
    exogen_fun = NULL, exogen_args = NULL,
    ...) {
  # Setup:
  k <- length(data_each)
  cols_each <- cols_each %||% purrr::map(data_each, colnames)
  var_args <- rep_args(k, list(...))

  exogen_each <- map_exogen(data_each, exogen_fun, exogen_args, k)

  purrr::pmap(list(data_each, p_each, exogen_each, var_args, cols_each),
    function(data, p, exogen, args, cols) {
      data <- dplyr::select(data, dplyr::all_of(cols))
      inject(vars::VAR(data, p, exogen = exogen, !!!args))
  })
}


#' Title
#'
#' @param data_each
#' @param p_each
#' @param criteria
#' @param cols_each
#' @param exogen_fun
#' @param exogen_args
#' @param star
#' @param star_args
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @export
mapvar_varselect <- function(
    data_each, p_each, cols_each = NULL,
    ..., criteria = c("AIC", "HQ", "SC", "FPE"),
    exogen_fun = NULL, exogen_args = NULL,
    star = FALSE, star_args = list()) {
  # Setup:
  k <- length(data_each)
  criteria <- paste0(criteria, "(n)")
  cols_each <- cols_each %||% purrr::map(data_each, colnames)

  var_args <- rep_args(k, list(...))
  exogen_list <- map_exogen(data, exogen_fun, exogen_args, k)

  result <- purrr::pmap(list(data_each, p_each, exogen_list, var_args, cols_each),
                        function(data, p, exogen, args, cols) {
                          data <- dplyr::select(data, dplyr::all_of(cols))
                          inject(vars::VARselect(data, p, exogen = exogen, !!!args)) %>%
                            purrr::map2(list(\(x)x[criteria], \(x)x[criteria,]), ~ .y(.x))
                        })

  if (star) {
    purrr::transpose(result)$selection %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(Model = names(result), .before = 1) %>%
      {inject(stargazer::stargazer(., summary = FALSE, !!!star_args))}
    invisible(result)
  } else {
    result
  }
}


#' Title
#'
#' @param select_each
#' @param model_labs
#' @param ...
#'
#' @return
#'
#' @examples
#'
#' @export
ggvar_select_patch <- function(select_each, model_labs = names(select_each), ...) {
  criteria <- gsub("\\(n\\)", "", names(select_each[[1]]$selection))

  plots <- purrr::map2(select_each, model_labs, function(x, name) {
    varutils::ggvar_select(x, criteria, ...) +
      ggplot2::labs(title = paste("Criteria for model:", name))
  })

  patchwork::wrap_plots(plots, guides = "collect")
}
