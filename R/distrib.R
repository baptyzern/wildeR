#' Statistical Distribution Summary
#'
#' This function computes various descriptive statistics for the given variable.
#'
#' @param var Vector of numeric data.
#' @param precision A character specifying the level of precision for the statistics.
#'   Options: "summary" or "S", "edges" or "E", "deciles" or "D", "deciles_edges" or "D+E". Default is "summary".
#' @param getLabels Logical argument - Should the labels of the variables be displayed in a message when printing the result of the function?
#' @param useNA When "message", shows the information about NAs in the variables in a message. When "include", shows it inside the returned data.frame.
#'
#' @return A data.frame object containing the selected descriptive statistics.
#'
#' The function calculates the following statistics based on the precision option:
#'   - "summary": min, p25, p50 (median), p75, max, mean, sd
#'   - "edges": min, p01, p05, p10, p25, p50, p75, p90, p95, p99, max, mean, sd
#'   - "deciles": min, p10, p20, p30, p40, p50, p60, p70, p80, p90, max, mean, sd
#'   - "deciles_edges": min, p01, p05, p10, p20, p30, p40, p50, p60, p70, p80, p90, p95, p99, max, mean, sd
#'
#'   If useNA is set to "include", additional information about NAs is included in the returned data.frame.
#'   - "n_NA": Count of missing values
#'   - "p_NA": Proportion of missing values
#'
#' @examples
#' # Example using the default precision ("summary")
#' data <- rnorm(100)
#' distrib(data)
#'
#' # Example using precision "edges"
#' data <- rnorm(100)
#' distrib(data, precision = "edges")
#'
#' @export
distrib <- function(var, precision = "summary", getLabel = T, useNA = "message")  {

  n_valid <- sum(!is.na(var))
  n_total <- sum(is.na(var)) + sum(!is.na(var))
  p_invalid <- 1 - n_valid / n_total

  if (getLabel & length(labelled::var_label(var)) > 0) {
    message(paste0("Variable : ", labelled::var_label(var)))
  }

  if (useNA == "message") {
    message(paste0("n_total in var = ", n_total))
    message(paste0("n_valid in var = ", n_valid))
    message(paste0("p_invalid in var = ", p_invalid))}

  if (precision == "S")   {precision <- "summary"}
  if (precision == "E")   {precision <- "edges"}
  if (precision == "D")   {precision <- "deciles"}
  if (precision == "D+E") {precision <- "deciles_edges"}
  if (precision == "E+D") {precision <- "deciles_edges"}

  if (precision == "summary") {
    rep <- cbind("min" = stats::quantile(var, probs = 0.00, na.rm = TRUE),
                 "p25" = stats::quantile(var, probs = 0.25, na.rm = TRUE, type = 1),
                 "p50" = stats::quantile(var, probs = 0.50, na.rm = TRUE, type = 1),
                 "p75" = stats::quantile(var, probs = 0.75, na.rm = TRUE, type = 1),
                 "max" = stats::quantile(var, probs = 1.00, na.rm = TRUE),
                 "mean"= base::mean(var, na.rm = TRUE),
                 "sd"  = stats::sd(var, na.rm = TRUE)
    )}


  if (precision == "edges") {
    rep <- cbind("min" = stats::quantile(var, probs = 0.00, na.rm = TRUE),
                 "p01" = stats::quantile(var, probs = 0.01, na.rm = TRUE, type = 1),
                 "p05" = stats::quantile(var, probs = 0.05, na.rm = TRUE, type = 1),
                 "p10" = stats::quantile(var, probs = 0.10, na.rm = TRUE, type = 1),
                 "p25" = stats::quantile(var, probs = 0.25, na.rm = TRUE, type = 1),
                 "p50" = stats::quantile(var, probs = 0.50, na.rm = TRUE, type = 1),
                 "p75" = stats::quantile(var, probs = 0.75, na.rm = TRUE, type = 1),
                 "p90" = stats::quantile(var, probs = 0.90, na.rm = TRUE, type = 1),
                 "p95" = stats::quantile(var, probs = 0.95, na.rm = TRUE, type = 1),
                 "p99" = stats::quantile(var, probs = 0.99, na.rm = TRUE, type = 1),
                 "max" = stats::quantile(var, probs = 1.00, na.rm = TRUE),
                 "mean"= base::mean(var, na.rm = TRUE),
                 "sd"  = stats::sd(var, na.rm = TRUE)
                 )
    }

  if (precision == "deciles") {
    rep <- cbind("min" = stats::quantile(var, probs = 0.00, na.rm = TRUE),
                 "p10" = stats::quantile(var, probs = 0.10, na.rm = TRUE, type = 1),
                 "p20" = stats::quantile(var, probs = 0.20, na.rm = TRUE, type = 1),
                 "p30" = stats::quantile(var, probs = 0.30, na.rm = TRUE, type = 1),
                 "p40" = stats::quantile(var, probs = 0.40, na.rm = TRUE, type = 1),
                 "p50" = stats::quantile(var, probs = 0.50, na.rm = TRUE, type = 1),
                 "p60" = stats::quantile(var, probs = 0.60, na.rm = TRUE, type = 1),
                 "p70" = stats::quantile(var, probs = 0.70, na.rm = TRUE, type = 1),
                 "p80" = stats::quantile(var, probs = 0.80, na.rm = TRUE, type = 1),
                 "p90" = stats::quantile(var, probs = 0.90, na.rm = TRUE, type = 1),
                 "max" = stats::quantile(var, probs = 1.00, na.rm = TRUE),
                 "mean"= base::mean(var, na.rm = TRUE),
                 "sd"  = stats::sd(var, na.rm = TRUE)
    )
  }

  if (precision == "deciles_edges") {
    rep <- cbind("min" = stats::quantile(var, probs = 0.00, na.rm = TRUE),
                 "p01" = stats::quantile(var, probs = 0.01, na.rm = TRUE, type = 1),
                 "p05" = stats::quantile(var, probs = 0.05, na.rm = TRUE, type = 1),
                 "p10" = stats::quantile(var, probs = 0.10, na.rm = TRUE, type = 1),
                 "p20" = stats::quantile(var, probs = 0.20, na.rm = TRUE, type = 1),
                 "p30" = stats::quantile(var, probs = 0.30, na.rm = TRUE, type = 1),
                 "p40" = stats::quantile(var, probs = 0.40, na.rm = TRUE, type = 1),
                 "p50" = stats::quantile(var, probs = 0.50, na.rm = TRUE, type = 1),
                 "p60" = stats::quantile(var, probs = 0.60, na.rm = TRUE, type = 1),
                 "p70" = stats::quantile(var, probs = 0.70, na.rm = TRUE, type = 1),
                 "p80" = stats::quantile(var, probs = 0.80, na.rm = TRUE, type = 1),
                 "p90" = stats::quantile(var, probs = 0.90, na.rm = TRUE, type = 1),
                 "p95" = stats::quantile(var, probs = 0.95, na.rm = TRUE, type = 1),
                 "p99" = stats::quantile(var, probs = 0.99, na.rm = TRUE, type = 1),
                 "max" = stats::quantile(var, probs = 1.00, na.rm = TRUE),
                 "mean"= base::mean(var, na.rm = TRUE),
                 "sd"  = stats::sd(var, na.rm = TRUE)
    )
  }

  if (useNA == "include") {
    rep <- cbind(rep,
                 "n_NA" = n_total - n_valid,
                 "p_NA" = p_invalid)
  }

  rownames(rep) <- NULL
  return(rep)
}
