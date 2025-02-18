% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_tabs.R
\name{cross_tabs}
\alias{cross_tabs}
\title{Generate Cross Tabs for Multiple Variables}
\usage{
cross_tabs(data, ...)
}
\arguments{
\item{data}{A data frame containing the data.}

\item{...}{One or more unquoted expressions representing the names of the variables for which the proportion tables should be generated.
If no variables are specified, the function will generate proportion tables for all variables in the dataset.}
}
\value{
A data frame with the proportion tables for the specified variables.
}
\description{
The \code{cross_tabs} function generates a proportion table for multiple variables within a dataset.
It uses an internal function to generate proportion tables for each specified variable and combines them into a single data frame.
}
\examples{
\dontrun{
data <- data.frame(
  gender = c("male", "female", "male", "female", "female"),
  age_group = c("young", "young", "old", "old", "young")
)
cross_tabs(data, gender, age_group)
}
}
