#' Crossed Graphical Distribution
#'
#' This function generates a crossed density plot to visualize the distribution of two variables.
#'
#' @param var1 A vector of numeric data for the first variable.
#' @param var2 A vector of categorial data for the second variable.
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
#' # Exemple d'utilisation de la fonction
#' data1 <- rnorm(100)
#' data2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' graphDistribCroisee(data1, data2)
#'
#' @export
graphDistribCroisee <- function(var1, var2) {

  n_total1 <- sum(is.na(var1)) + sum(!is.na(var1))
  n_valid1 <- sum(!is.na(var1))
  p_invalid1 <- 1 - n_valid1/n_total1

  n_total2 <- sum(is.na(var2)) + sum(!is.na(var2))
  n_valid2 <- sum(!is.na(var2))
  p_invalid2 <- 1 - n_valid2/n_total2

  if (length(labelled::var_label(var1)) > 0) {
    message("\n")
    message(paste0("Variable 1 : ", labelled::var_label(var1)))
  }

  message("\n")
  message(paste0("n_total in var1 = ", n_total1))
  message(paste0("n_valid in var1 = ", n_valid1))
  message(paste0("p_invalid in var1 = ", p_invalid1))

  if (length(labelled::var_label(var2)) > 0) {
    message("\n")
    message(paste0("Variable 2 : ", labelled::var_label(var2)))
  }
  message("\n")
  message(paste0("n_total in var2 = ", n_total2))
  message(paste0("n_valid in var2 = ", n_valid2))
  message(paste0("p_invalid in var2 = ", p_invalid2))
  message("\n")
  message("Be aware of the distribution of NA among groups!")
  message("\n")

  data <- data.frame(value1 = var1, value2 = var2)
  plot <- ggplot(data = data, aes(x = value1, group = value2, color = value2)) +
    geom_density(linewidth = 0.7)
  return(plot)
}
