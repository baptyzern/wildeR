% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphDistribCroisee.R
\name{graphDistribCroisee}
\alias{graphDistribCroisee}
\title{Crossed Graphical Distribution}
\usage{
graphDistribCroisee(
  var1,
  var2,
  output = "graph",
  getLabels = T,
  warnNA = T,
  ...
)
}
\arguments{
\item{var1}{A vector of numeric data for the first variable.}

\item{var2}{A vector of categorial data for the second variable.}

\item{output}{Specifies the output type. Options: "graph" (default) or "layer".}

\item{getLabels}{Logical. If TRUE, includes variable labels in the output messages.}

\item{warnNA}{Logical. If TRUE, includes warnings about NA values in the output messages.}

\item{...}{Additional arguments to be passed to ggplot function.}
}
\value{
A ggplot object representing the crossed density plot.

The function also prints information about the variables, including the total number of observations,
the number of valid observations, the proportion of invalid values, and a reminder to be aware of the
distribution of NA values among groups.
}
\description{
This function generates a crossed density plot to visualize the distribution of two variables.
}
\examples{
# Example using the function
data1 <- rnorm(100)
data2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
graphDistribCroisee(data1, data2)

}
