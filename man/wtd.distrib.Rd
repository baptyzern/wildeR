% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wtd.distrib.R
\name{wtd.distrib}
\alias{wtd.distrib}
\title{Title}
\usage{
wtd.distrib(x, y = TRUE, w, filtering = TRUE, probs = c(0.1 * 0:10))
}
\arguments{
\item{filtering}{}
}
\description{
Title
}
\examples{
sample <- data.frame(
 variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female", "Male"),
 variable_y =      c(1,        2,        3, NA, NA,      4,        1,    2),
 weights =         c(1,        1,        3,  1,  2,      4,        1,    NA))
}
