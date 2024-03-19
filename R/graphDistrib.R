#' Graphical Distribution Summary
#'
#' This function generates a density plot to visualize the distribution of a numeric variable.
#'
#' @param var A vector of numeric data.
#' @param var2 A vector of qualitative data (if you desire to compute statistics by groups).
#' @param output Character specifying the output type. Options: "graph" for a full ggplot object, "layer" to add the density layer to an existing plot.
#' @param getLabels Logical argument - Should the labels of the variables be displayed in a message when printing the result of the function?
#' @param warnNA Logical argument - Should information about NAs be printed in a message?
#' @param filter A logical vector used to filter `var` (and `var2`). Must be the same lenght.
#' @param ... Additional arguments to be passed to the `geom_density` function from ggplot2.
#'
#' @return A ggplot object representing the density plot if output is "graph". If output is "layer", it adds the density layer to an existing plot.
#'
#' The function also prints information about the variable, including the total number of observations,
#' the number of valid (non-NA) values, and the proportion of invalid (NA) values, if warnNA is set to TRUE.
#'
#' @examples
#' # Example using a numeric vector
#' data_ex <- rnorm(100)
#' graphDistrib(data_ex)
#'
#' # Example using the `var2` argument
#' data_ex1 <- rnorm(100)
#' data_ex2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' graphDistrib(data_ex1, data_ex2)
#'
#' # Example using the `filter` argument
#' data_ex <- rnorm(100)
#' graphDistrib(data_ex, filter = data_ex > 0)
#'
#' # Example using the `output` argument set to layer
#' library(ggplot2)
#' data_ex <- rnorm(100)
#' ggplot() + graphDistrib(data_ex, filter = data_ex > 0, output = "layer")

#' @import ggplot2
#' @export

graphDistrib <- function(var, var2 = NA, filter = NA, output = "graph", getLabels = TRUE, warnNA = TRUE, ...) {

# Checking if var2 exists (=/= NA)
  if (all(is.na(var2))) {

  # Case 1 - var2 doesn't exist, or is filled with NA only

    # If filter is not filled with only NA, filter is applied

    if (!all(is.na(filter))) {

      # Error is displayed when filter is not the same length as var.
      if (length(filter) != length(var)) {stop("filter is not the same length as var.")}
      message(paste0("Filtering ", length(var) - length(var[filter]), " observations.\n"))
      var <- var[filter]
    }

    # Displaying vector label if it exists and getLabels = TRUE
    if (getLabels & length(labelled::var_label(var)) > 0) {
      message(paste0("Variable : ", labelled::var_label(var)))
    }
    # Computing and displaying NA stats if warnNA = TRUE
    n_valid <- sum(!is.na(var))
    n_total <- sum(is.na(var)) + sum(!is.na(var))
    p_invalid <- 1 - n_valid / n_total

    if (warnNA) {
      message(paste0("n_total in var = ", n_total))
      message(paste0("n_valid in var = ", n_valid))
      message(paste0("p_invalid in var = ", p_invalid))
    }

    # Outputing to graph directly or to ggplot layer according to output argument

    if (output == "graph") {
      plot <- ggplot() +
        geom_density(data = data.frame(value = var), aes(x = value), ...)
      return(plot)
    }
    if (output == "layer") {
      geom_density(data = data.frame(value = var), aes(x = value), ...)
    }

  } else {

    # Case 2 - var2 exists and is filled with NA only
    var1 <- var


    if (!all(is.na(filter))) {
      if (length(filter) != length(var1)) {stop("filter is not the same length as var.")}
      if (length(filter) != length(var1)) {stop("filter is not the same length as var.")}
      message(paste0("Filtered ", length(var1) - length(var1[filter]), " observations. \n"))
      var1 <- var1[filter]
      var2 <- var2[filter]
    }

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

}
