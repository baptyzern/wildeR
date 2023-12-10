distrib <- function(var, large = FALSE) {
  if (large) {
    rep <- cbind("min" = quantile(var, probs = 0.00, na.rm = T),
                 "p01" = quantile(var, probs = 0.01, na.rm = T, type = 1),
                 "p05" = quantile(var, probs = 0.05, na.rm = T, type = 1),
                 "p20" = quantile(var, probs = 0.20, na.rm = T, type = 1),
                 "p25" = quantile(var, probs = 0.25, na.rm = T, type = 1),
                 "p40" = quantile(var, probs = 0.40, na.rm = T, type = 1),
                 "p50" = quantile(var, probs = 0.50, na.rm = T, type = 1),
                 "p60" = quantile(var, probs = 0.60, na.rm = T, type = 1),
                 "p75" = quantile(var, probs = 0.75, na.rm = T, type = 1),
                 "p80" = quantile(var, probs = 0.80, na.rm = T, type = 1),
                 "p95" = quantile(var, probs = 0.95, na.rm = T, type = 1),
                 "p99" = quantile(var, probs = 0.99, na.rm = T, type = 1),
                 "max" = quantile(var, probs = 1.00, na.rm = T),
                 "mean"= mean(var, na.rm = T),
                 "sd"  = sd(var, na.rm = T),
                 "n"     = sum(is.na(var)) + sum(!is.na(var)),
                 "NA(n)" = sum(is.na(var)),
                 "NA(%)" = round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3)
    )} else {
      rep <- cbind("min" = quantile(var, probs = 0.00, na.rm = T),
                   "p25" = quantile(var, probs = 0.25, na.rm = T, type = 1),
                   "p50" = quantile(var, probs = 0.50, na.rm = T, type = 1),
                   "p75" = quantile(var, probs = 0.75, na.rm = T, type = 1),
                   "max" = quantile(var, probs = 1.00, na.rm = T),
                   "mean"= mean(var, na.rm = T),
                   "sd"  = sd(var, na.rm = T),
                   "n"     = sum(is.na(var)) + sum(!is.na(var)),
                   "NA(n)" = sum(is.na(var)),
                   "NA(%)" = round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3)
      )}


  rownames(rep) <- NULL
  return(rep)
}
