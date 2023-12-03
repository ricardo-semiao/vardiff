#' Title
#'
#' @param select_list
#' @param model_names
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggmap_varselect <- function(select_list, model_names = names(select_list), ...) {
  criteria <- gsub("\\(n\\)", "", names(select_list[[1]]$selection))

  plots <- purrr::map2(select_list, model_names, function(x, name) {
    varutils::ggvar_select(x, criteria, ...) +
      ggplot2::labs(title = paste("Criteria for model:", name))
  })

  patchwork::wrap_plots(plots, guides = "collect")
}
