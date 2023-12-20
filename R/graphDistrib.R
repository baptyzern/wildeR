#' Graphical Distribution Summary
#'
#' This function generates a density plot to visualize the distribution of a numeric variable.
#'
#' @param var A vector of numeric data.
#'
#' @return A ggplot object representing the density plot.
#'
#' The function also prints information about the variable, including the total number of observations,
#' the number of valid (non-NA) values, and the proportion of invalid (NA) values.
#'
#' @examples
#' # Example using a numeric vector
#' data <- rnorm(100)
#' graphDistrib(data)
#'
#' @import ggplot2
#' @export
graphDistrib <- function(var) {

  n_valid <- sum(!is.na(var))
  n_total <- sum(is.na(var)) + sum(!is.na(var))
  p_invalid <- 1 - n_valid / n_total

  if (length(labelled::var_label(var)) > 0) {
    message("\n")
    message(paste0("Variable : ", labelled::var_label(var)))
  }

  message("\n")
  message(paste0("n_total in var = ", n_total))
  message(paste0("n_valid in var = ", n_valid))
  message(paste0("p_invalid in var = ", p_invalid))
  message("\n")

  data <- data.frame(value = var)
  plot <- ggplot(data = data, aes(x = value)) + geom_density()
  return(plot)
}
