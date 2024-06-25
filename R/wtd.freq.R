#' Weighted frequency tables.
#'
#' Generate and format weighted frequency tables from a variable or a table, with percentages and formatting options.
#'
#' @param x either a vector to be tabulated, or a table object
#' @param weights vector of weights, must be the same length as `x`
#' @param useNA in "always", "no", "ifany" (default)
#'
#' @return The result is an object of class data.frame.
#' @export
#'
#' @examples
#' sample <- data.frame(
#'  variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female", "Male"),
#'  weights =         c(1,        1,        3,  1,  2,      4,        1,    NA))
#' wtd.freq(sample$variable_x, sample$weights)

wtd.freq <- function(x, weights, filter = TRUE, useNA = "ifany") {
  x <- x[filter]
  weights <- weights[filter]
  unwtd_n <- table(x, useNA = useNA) |> addmargins()
  wtd_p <- questionr::wtd.table(x, weights = weights, useNA = useNA) |>
    questionr::freq(total = T) |>
    dplyr::pull("%")

  return(cbind(unwtd_n, wtd_p))
}
