#' @title Calculate Proportion Table for a Single Variable
#' @description Helper function to calculate counts and proportions, preserving empty factor levels.
calc_prop_table <- function(data, var, digits, na.rm) {

  # Safe label extraction (falls back to variable name if no label exists)
  var_lab <- attr(data[[var]], "label")
  if (is.null(var_lab)) var_lab <- var

  # Calculation
  # Use .data[[var]] for count (data masking verb)
  res <- data |>
    dplyr::count(.data[[var]], name = "sub_n", .drop = FALSE)

  # Use all_of(var) for drop_na (tidyselect verb)
  if (na.rm) {
    res <- res |> tidyr::drop_na(dplyr::all_of(var))
  }

  res |>
    dplyr::rename(var_level = dplyr::all_of(var)) |>
    dplyr::mutate(
      total_n = sum(sub_n),
      prop = round(sub_n / total_n, digits),
      variable = var,
      variable_label = as.character(var_lab),
      var_level = as.character(var_level)
    ) |>
    dplyr::select(
      variable,
      variable_label,
      var_level,
      total_n,
      sub_n,
      prop
    )
}
