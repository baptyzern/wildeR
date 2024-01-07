#' Graphical Distribution Summary
#'
#' This function generates a density plot to visualize the distribution of a numeric variable.
#'
#' @param var A vector of numeric data.
#' @param output Character specifying the output type. Options: "graph" for a full ggplot object, "layer" to add the density layer to an existing plot.
#' @param getLabel Logical argument - Should the label of the variable be displayed in a message when printing the result of the function?
#' @param warnNA Logical argument - Should information about NAs be printed in a message?
#' @param ... Additional arguments to be passed to the `geom_density` function from ggplot2.
#'
#' @return A ggplot object representing the density plot if output is "graph". If output is "layer", it adds the density layer to an existing plot.
#'
#' The function also prints information about the variable, including the total number of observations,
#' the number of valid (non-NA) values, and the proportion of invalid (NA) values, if warnNA is set to TRUE.
#'
#' @examples
#' # Example using a numeric vector
#' data <- rnorm(100)
#' graphDistrib(data)
#'
#' @import ggplot2
#' @export
graphDistrib <- function(var, output = "graph", getLabel = T, warnNA = T, ...) {

  n_valid <- sum(!is.na(var))
  n_total <- sum(is.na(var)) + sum(!is.na(var))
  p_invalid <- 1 - n_valid / n_total

  if (getLabel & length(labelled::var_label(var)) > 0) {
    message(paste0("Variable : ", labelled::var_label(var)))
  }

  if (warnNA) {
    message(paste0("n_total in var = ", n_total))
    message(paste0("n_valid in var = ", n_valid))
    message(paste0("p_invalid in var = ", p_invalid))
  }

  if (output == "graph") {
    plot <- ggplot() +
      geom_density(data = data.frame(value = var), aes(x = value), ...)
    return(plot)
  }

  if (output == "layer") {
    geom_density(data = data.frame(value = var), aes(x = value), ...)
  }
}
