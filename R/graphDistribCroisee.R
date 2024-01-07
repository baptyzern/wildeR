#' Crossed Graphical Distribution
#'
#' This function generates a crossed density plot to visualize the distribution of two variables.
#'
#' @param var1 A vector of numeric data for the first variable.
#' @param var2 A vector of categorial data for the second variable.
#' @param output Specifies the output type. Options: "graph" (default) or "layer".
#' @param getLabels Logical. If TRUE, includes variable labels in the output messages.
#' @param warnNA Logical. If TRUE, includes warnings about NA values in the output messages.
#' @param ... Additional arguments to be passed to ggplot function.
#'
#' @return A ggplot object representing the crossed density plot.
#'
#' The function also prints information about the variables, including the total number of observations,
#' the number of valid observations, the proportion of invalid values, and a reminder to be aware of the
#' distribution of NA values among groups.
#'
#' @import ggplot2
#'
#' @examples
#' # Example using the function
#' data1 <- rnorm(100)
#' data2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' graphDistribCroisee(data1, data2)
#'
#' @export

graphDistribCroisee <- function(var1, var2, output = "graph", getLabels = T, warnNA = T, ...) {

  n_total1 <- sum(is.na(var1)) + sum(!is.na(var1))
  n_valid1 <- sum(!is.na(var1))
  p_invalid1 <- 1 - n_valid1/n_total1

  n_total2 <- sum(is.na(var2)) + sum(!is.na(var2))
  n_valid2 <- sum(!is.na(var2))
  p_invalid2 <- 1 - n_valid2/n_total2

  if (getLabels & length(labelled::var_label(var1)) > 0) {message(paste0("Variable 1 : ", labelled::var_label(var1)))}

  if (warnNA) {
    message(paste0("n_total in var1 = ", n_total1))
    message(paste0("n_valid in var1 = ", n_valid1))
    message(paste0("p_invalid in var1 = ", p_invalid1))}

  if (getLabels & length(labelled::var_label(var2)) > 0) {message(paste0("Variable 2 : ", labelled::var_label(var2)))}


  if (warnNA) {
    message(paste0("n_total in var2 = ", n_total2))
    message(paste0("n_valid in var2 = ", n_valid2))
    message(paste0("p_invalid in var2 = ", p_invalid2))
    message("Be aware of the distribution of NA among groups!")}

  if (output == "graph") {
    plot <- ggplot() +
      geom_density(data = data.frame(value1 = var1, value2 = var2), aes(x = value1, group = value2, color = value2), ...)
    return(plot)
  }

  if (output == "layer") {
    geom_density(data = data.frame(value1 = var1, value2 = var2), aes(x = value1, group = value2, color = value2), ...)
  }

}
