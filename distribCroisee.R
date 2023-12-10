distribCroisee <- function(var1, var2, large = FALSE) {
  if (large) {
    rep <- cbind("min" = tapply(var1, var2, quantile, probs = 0.00, na.rm = T),
                 "p01" = tapply(var1, var2, quantile, probs = 0.01, na.rm = T, type = 1),
                 "p05" = tapply(var1, var2, quantile, probs = 0.05, na.rm = T, type = 1),
                 "p20" = tapply(var1, var2, quantile, probs = 0.20, na.rm = T, type = 1),
                 "p25" = tapply(var1, var2, quantile, probs = 0.25, na.rm = T, type = 1),
                 "p40" = tapply(var1, var2, quantile, probs = 0.40, na.rm = T, type = 1),
                 "p50" = tapply(var1, var2, quantile, probs = 0.50, na.rm = T, type = 1),
                 "p60" = tapply(var1, var2, quantile, probs = 0.60, na.rm = T, type = 1),
                 "p75" = tapply(var1, var2, quantile, probs = 0.75, na.rm = T, type = 1),
                 "p80" = tapply(var1, var2, quantile, probs = 0.80, na.rm = T, type = 1),
                 "p95" = tapply(var1, var2, quantile, probs = 0.95, na.rm = T, type = 1),
                 "p99" = tapply(var1, var2, quantile, probs = 0.99, na.rm = T, type = 1),
                 "max" = tapply(var1, var2, quantile, probs = 1.00, na.rm = T),
                 "mean"= tapply(var1, var2, mean, na.rm = T),
                 "sd"  = tapply(var1, var2, sd, na.rm = T),
                 "n"     = tapply(var1, var2, function(var) {return(sum(is.na(var)) + sum(!is.na(var)))}),
                 "NA(n)" = tapply(var1, var2, function(var) {return(sum(is.na(var)))}),
                 "NA(%)" = tapply(var1, var2, function(var) {return(round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3))})
    )} else {
      rep <- cbind("min" = tapply(var1, var2, quantile, probs = 0.00, na.rm = T),
                   "p25" = tapply(var1, var2, quantile, probs = 0.25, na.rm = T, type = 1),
                   "p50" = tapply(var1, var2, quantile, probs = 0.50, na.rm = T, type = 1),
                   "p75" = tapply(var1, var2, quantile, probs = 0.75, na.rm = T, type = 1),
                   "max" = tapply(var1, var2, quantile, probs = 1.00, na.rm = T),
                   "mean"= tapply(var1, var2, mean, na.rm = T),
                   "sd"  = tapply(var1, var2, sd, na.rm = T),
                   "n"     = tapply(var1, var2, function(var) {return(sum(is.na(var)) + sum(!is.na(var)))}),
                   "NA(n)" = tapply(var1, var2, function(var) {return(sum(is.na(var)))}),
                   "NA(%)" = tapply(var1, var2, function(var) {return(round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3))})
      )}



  return(rep)
}
