#' Crossed Statistical Distribution Summary
#'
#' This function computes various descriptive statistics for two variables and their cross-tabulation.
#'
#' @param var1 First vector of numeric data.
#' @param var2 Second vector of categorial data.
#' @param precision A character specifying the level of precision for the statistics.
#'   Options: "summary" or "S", "edges" or "E", "deciles" or "D", "deciles_edges" or "D+E". Default is "summary".
#' @param getLabels Logical argument - Should the labels of the variables be displayed in a message when printing the result of the function?
#' @param useNA When "message", shows the information about NAs in the variables in a message. When "include", shows it inside the returned data.frame.
#'
#' @return A data.frame object containing the selected descriptive statistics for the crossed variables.
#'
#' The function calculates the following statistics based on the precision option:
#'   - "summary": min, p25, p50 (median), p75, max, mean, sd, n_valid, p_invalid
#'   - "edges": min, p01, p05, p10, p25, p50, p75, p90, p95, p99, max, mean, sd
#'   - "deciles": min, p10, p20, p30, p40, p50, p60, p70, p80, p90, max, mean, sd
#'   - "deciles_edges": min, p01, p05, p10, p20, p30, p40, p50, p60, p70, p80, p90, p95, p99, max, mean, sd
#'
#'   If useNA is set to "include", additional information about NAs is included in the returned data.frame.
#'   - "n_valid": Count of non-missing values
#'   - "p_NA": Proportion of missing values
#'
#' @examples
#' # Example using the default precision ("summary")
#' data1 <- rnorm(100)
#' data2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' distribCroisee(data1, data2)
#'
#' # Example using precision "edges"
#' data1 <- rnorm(100)
#' data2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' distribCroisee(data1, data2, precision = "edges")
#'
#' @export

distribCroisee <- function(var1, var2, precision = "summary", getLabels = T, useNA = "message") {

  n_total1 <- sum(is.na(var1)) + sum(!is.na(var1))
  n_valid1 <- sum(!is.na(var1))
  p_invalid1 <- 1 - n_valid1/n_total1

  n_total2 <- sum(is.na(var2)) + sum(!is.na(var2))
  n_valid2 <- sum(!is.na(var2))
  p_invalid2 <- 1 - n_valid2/n_total2

  if (getLabels & length(labelled::var_label(var1)) > 0) {message(paste0("Variable 1 : ", labelled::var_label(var1)))}

  if (useNA == "message") {
  message(paste0("n_total in var1 = ", n_total1))
  message(paste0("n_valid in var1 = ", n_valid1))
  message(paste0("p_invalid in var1 = ", p_invalid1))}

  if (getLabels & length(labelled::var_label(var2)) > 0) {message(paste0("Variable 2 : ", labelled::var_label(var2)))}

  if (useNA == "message") {
  message(paste0("n_total in var2 = ", n_total2))
  message(paste0("n_valid in var2 = ", n_valid2))
  message(paste0("p_invalid in var2 = ", p_invalid2))
  message("Be aware of the distribution of NA among groups!")}

  if (precision == "S")   {precision <- "summary"}
  if (precision == "E")   {precision <- "edges"}
  if (precision == "D")   {precision <- "deciles"}
  if (precision == "D+E") {precision <- "deciles_edges"}
  if (precision == "E+D") {precision <- "deciles_edges"}

  if (precision == "summary") {
    rep <- cbind("min" = tapply(var1, var2, stats::quantile, probs = 0.00, na.rm = T),
                 "p25" = tapply(var1, var2, stats::quantile, probs = 0.25, na.rm = T, type = 1),
                 "p50" = tapply(var1, var2, stats::quantile, probs = 0.50, na.rm = T, type = 1),
                 "p75" = tapply(var1, var2, stats::quantile, probs = 0.75, na.rm = T, type = 1),
                 "max" = tapply(var1, var2, stats::quantile, probs = 1.00, na.rm = T),
                 "mean"= tapply(var1, var2, base::mean, na.rm = T),
                 "sd"  = tapply(var1, var2, stats::sd, na.rm = T)
                 )
    }

  if (precision == "edges") {
    rep <- cbind("min" = tapply(var1, var2, stats::quantile, probs = 0.00, na.rm = T),
                 "p01" = tapply(var1, var2, stats::quantile, probs = 0.01, na.rm = T, type = 1),
                 "p05" = tapply(var1, var2, stats::quantile, probs = 0.05, na.rm = T, type = 1),
                 "p25" = tapply(var1, var2, stats::quantile, probs = 0.25, na.rm = T, type = 1),
                 "p50" = tapply(var1, var2, stats::quantile, probs = 0.50, na.rm = T, type = 1),
                 "p75" = tapply(var1, var2, stats::quantile, probs = 0.75, na.rm = T, type = 1),
                 "p95" = tapply(var1, var2, stats::quantile, probs = 0.95, na.rm = T, type = 1),
                 "p99" = tapply(var1, var2, stats::quantile, probs = 0.99, na.rm = T, type = 1),
                 "max" = tapply(var1, var2, stats::quantile, probs = 1.00, na.rm = T),
                 "mean"= tapply(var1, var2, base::mean, na.rm = T),
                 "sd"  = tapply(var1, var2, stats::sd, na.rm = T)
                 )
  }

  if (precision == "deciles") {
    rep <- cbind("min" = tapply(var1, var2, stats::quantile, probs = 0.00, na.rm = T),
                 "p10" = tapply(var1, var2, stats::quantile, probs = 0.10, na.rm = T, type = 1),
                 "p20" = tapply(var1, var2, stats::quantile, probs = 0.20, na.rm = T, type = 1),
                 "p30" = tapply(var1, var2, stats::quantile, probs = 0.30, na.rm = T, type = 1),
                 "p40" = tapply(var1, var2, stats::quantile, probs = 0.40, na.rm = T, type = 1),
                 "p50" = tapply(var1, var2, stats::quantile, probs = 0.50, na.rm = T, type = 1),
                 "p60" = tapply(var1, var2, stats::quantile, probs = 0.60, na.rm = T, type = 1),
                 "p70" = tapply(var1, var2, stats::quantile, probs = 0.70, na.rm = T, type = 1),
                 "p80" = tapply(var1, var2, stats::quantile, probs = 0.80, na.rm = T, type = 1),
                 "p90" = tapply(var1, var2, stats::quantile, probs = 0.90, na.rm = T, type = 1),
                 "max" = tapply(var1, var2, stats::quantile, probs = 1.00, na.rm = T),
                 "mean"= tapply(var1, var2, base::mean, na.rm = T),
                 "sd"  = tapply(var1, var2, stats::sd, na.rm = T)
    )
  }

  if (precision == "deciles_edges") {
    rep <- cbind("min" = tapply(var1, var2, stats::quantile, probs = 0.00, na.rm = T),
                 "p01" = tapply(var1, var2, stats::quantile, probs = 0.01, na.rm = T, type = 1),
                 "p05" = tapply(var1, var2, stats::quantile, probs = 0.05, na.rm = T, type = 1),
                 "p10" = tapply(var1, var2, stats::quantile, probs = 0.10, na.rm = T, type = 1),
                 "p20" = tapply(var1, var2, stats::quantile, probs = 0.20, na.rm = T, type = 1),
                 "p30" = tapply(var1, var2, stats::quantile, probs = 0.30, na.rm = T, type = 1),
                 "p40" = tapply(var1, var2, stats::quantile, probs = 0.40, na.rm = T, type = 1),
                 "p50" = tapply(var1, var2, stats::quantile, probs = 0.50, na.rm = T, type = 1),
                 "p60" = tapply(var1, var2, stats::quantile, probs = 0.60, na.rm = T, type = 1),
                 "p70" = tapply(var1, var2, stats::quantile, probs = 0.70, na.rm = T, type = 1),
                 "p80" = tapply(var1, var2, stats::quantile, probs = 0.80, na.rm = T, type = 1),
                 "p90" = tapply(var1, var2, stats::quantile, probs = 0.90, na.rm = T, type = 1),
                 "p95" = tapply(var1, var2, stats::quantile, probs = 0.95, na.rm = T, type = 1),
                 "p99" = tapply(var1, var2, stats::quantile, probs = 0.99, na.rm = T, type = 1),
                 "max" = tapply(var1, var2, stats::quantile, probs = 1.00, na.rm = T),
                 "mean"= tapply(var1, var2, base::mean, na.rm = T),
                 "sd"  = tapply(var1, var2, stats::sd, na.rm = T)
                 )
  }

  if (useNA == "include") {
    rep <- cbind(rep,
                 "n_valid" = tapply(var1, var2, function(x) {sum(!is.na(x))}),
                 "p_NA"    = tapply(var1, var2, function(x) {sum(is.na(x))/length(x)})
    )
  }

  return(rep)
}
