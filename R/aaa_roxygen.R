param_cols <- function() {
  glue(
    "@param cols_each A list of columns to consider for each data split. \\
    Defaults to everything."
  )
}

param_ggplot_args <- function(fun_name) {
  param <- ifelse(
    grepl("facet", fun_name),
    "args_facet",
    gsub("geom_(.+)", "\\1_args", fun_name)
  )

  glue(
    "@param {param} Aditional arguments passed to \\
    \\link[ggplot2]{{{fun_name}}}."
  )
}

param_colors <- function() {
  glue(
    "@param colors A vector of colors for each variable. Passed to \\
    \\link[ggplot2]{{scale_fill_manual}}. See \\
    \\code{{vignette(\"colors\", package = \"varutils\")}}."
  )
}

param_splits <- function() {
  glue(
    "@param splits_each A list of numeric vectors with split points: \\
    \\code{{list(c(start1, end1), ...)}}. Size one vectors are interpreted to \\
    have 'end' as \\code{{nrow(data)}}."
  )
}
