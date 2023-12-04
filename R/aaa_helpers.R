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
