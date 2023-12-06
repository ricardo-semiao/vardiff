utils::globalVariables(".") # Remove CRAN note towards the magrittr dot

# General helpers ---------------------------------------------------------
rep_args <- function(k, args) {
  if (length(args) == 0) {
    rep(list(list()), k)
  } else {
    purrr::imap(args, function(arg, name) {
      if (is.list(arg) && length(arg) == k) {
        purrr::set_names(arg, name)
      } else {
        rep(purrr::set_names(list(arg), name), k)
      }
    }) %>%
      purrr::pmap(list) %>%
      unname()
  }
}

search_map <- function(.l, .cond, .f) {
  if (.cond(.l)) {
    return(purrr::map(.l, ~ search_map(.x, .cond, .f)))
  } else {
    return(.f(.l))
  }
}



# Test helpers ------------------------------------------------------------
test_funs <- list(
  type = list(
    numeric = is.numeric,
    character = is.character,
    boolean = rlang::is_bool,
    list = is.list
  ),
  class = list(
    all = rlang::inherits_all,
    any = rlang::inherits_any,
    only = rlang::inherits_only
  )
)



test <- list(
  type_arg = function(arg, type, len = NULL, n = 2, msg = NULL) {
    arg_name <- rlang::ensym(arg)
    len_name <- rlang::enexpr(len)
    arg %||% return()

    if (is.null(len)) {
      len <- len %||% length(arg)
      len_text <- ""
    } else {
      len_text <- paste0(" with length ", len_name, " (", len, ")")
    }

    if (!(test_funs$type[[type]](arg) && length(arg) == len)) {
      cli_abort(
        msg %||% "`{arg_name}` must be {type}{len_text}.",
        call = caller_env(n = n)
      )
    }
  },
  class_arg = function(arg, class, what = "any", n = 2, msg = NULL) {
    arg_name <- rlang::ensym(arg)
    arg %||% return()

    if (!test_funs$class[[what]](arg, class)) {
      cli_abort(
        msg %||% "`{arg_name}` must inherit {what} {class}.",
        call = caller_env(n = n)
      )
    }
  },
  categorical_arg = function(arg, options, fmt = "\"", n = 2, msg = NULL) {
    arg_name <- rlang::ensym(arg)
    arg %||% return()

    if (!(arg %in% options)) {
      cli_abort(
        msg %||% "`{arg_name}` must be one of {paste0(fmt, options, fmt)}, not\\
         {paste0(fmt, arg, fmt)}`.",
        call = caller_env(n = n)
      )
    }
  },
  cols_each = function(arg, names, n = 2, msg = NULL) {
    if (!is.list(names)) names <- rep(list(names), length(arg))
    arg %||% return()

    if (!is.null(arg) && !all(purrr::map2_lgl(arg, names, ~ all(.x %in% .y)))) {
      cli_abort(
        msg %||% "Every `cols_each` element must contains only names of \\
        columns present in `data`.",
        call = caller_env(n = n)
      )
    }
  },
  splits_each = function(arg, size, n = 2, msg = rep(list(NULL), 2)) {
    cond1 <- any(purrr::map_lgl(arg,
      ~ any(!is.numeric(.x) & length(.x) > 2 & length(.x) == 0)
    ))
    cond2 <- !all(purrr::map_lgl(arg, ~ all(.x %in% 1:size)))

    if (cond1) {
      cli_abort(
        msg[[1]] %||% "Every `splits_each` element must be numeric of size 2 \\
        or 1.",
        call = caller_env(n = n)
      )
    }
    if (cond2) {
      cli_abort(
        msg[[2]] %||% "Every `splits_each` element must be within \\
        `1:nrow(data)` (1:{size})",
      call = caller_env(n = n)
      )
    }
  },
  fun_arg = function(arg, fun, n = 2, msg = NULL) {
    arg_name <- rlang::ensym(arg)
    if (paste(arg_name) == "dots") arg_name <- "..."
    fun_name <- rlang::enexpr(fun)
    cond1 <- names(arg) %in% names(formals(fun))

    if (!(all(cond1))) {
      cli_warn(
        msg %||% "`{arg_name}` has elements there aren't arguments of \\
        {fun_name}. Ignore if they're to be passed to `...`.",
        "*" = "Elements: {names(arg)[cond1]}",
        call = caller_env(n = n)
      )
    }
  }
)


