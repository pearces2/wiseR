# Internal function to generate proportion table for a given variable
inner_fns <- function(data, var) {

  data_var_label <-
    data |>
    pull(.data[[var]]) |>
    var_label()

  data |>
    count(.data[[var]],
          .drop = FALSE) |>
    drop_na() |>
    rename(var_level = all_of(var)) |>
    arrange(var_level) |>
    add_tally(n, name = 'total') |>
    mutate(
      prop = round(n/total, 2),
      var_level = var_level |> as.character(),
      variable = var,
      variable_label = data_var_label
    ) |>
    select(
      variable,
      variable_label,
      var_level,
      total_n = total,
      sub_n = n,
      prop
    )

}
