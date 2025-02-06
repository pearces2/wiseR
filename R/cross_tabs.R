#' @title Generate Cross Tabs for Multiple Variables
#' @description
#' The `cross_tabs` function generates a proportion table for multiple variables within a dataset.
#' It uses an internal function to generate proportion tables for each specified variable and combines them into a single data frame.
#'
#' @param data A data frame containing the data.
#' @param ... One or more unquoted expressions representing the names of the variables for which the proportion tables should be generated.
#' If no variables are specified, the function will generate proportion tables for all variables in the dataset.
#'
#' @return A data frame with the proportion tables for the specified variables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   gender = c("male", "female", "male", "female", "female"),
#'   age_group = c("young", "young", "old", "old", "young")
#' )
#' cross_tabs(data, gender, age_group)
#' }
cross_tabs <- function(data, ...) {

  vars <-
    data |>
    select(...) |>
    names()

  if (length(vars) == 0) {
    vars <-
      data |>
      names()
  }

  map(
    vars,
    ~inner_fns(data, .x)
  ) |>
    list_rbind()

}
