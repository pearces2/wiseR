
#' @title Generate Cross Tabs for Multiple Variables
#' @description Generates a proportion table for multiple variables within a dataset.
#'
#' @param data A data frame.
#' @param ... Variables to tabulate (supports tidyselect). If empty, selects all columns.
#' @param digits Integer. Number of decimal places for proportions. Defaults to 2.
#' @param na.rm Logical. Whether to remove NA values from the calculation. Defaults to TRUE.
#'
#' @return A tibble with summary statistics.
#' @export
cross_tabs <- function(data, ..., digits = 2, na.rm = TRUE) {

  # 1. Robust variable selection using tidyselect
  vars_loc <- tryCatch(
    tidyselect::eval_select(rlang::expr(c(...)), data),
    error = function(e) integer(0)
  )

  if (length(vars_loc) == 0) {
    vars <- names(data)
  } else {
    vars <- names(vars_loc)
  }

  # 2. Iterate and combine
  purrr::map(
    vars,
    \(x) calc_prop_table(data, x, digits = digits, na.rm = na.rm)
  ) |>
    purrr::list_rbind()
}
