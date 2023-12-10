#' Distribution croisée de statistiques descriptives
#'
#' Cette fonction calcule différentes statistiques descriptives croisées en fonction de l'argument `large`.
#'
#' @param var1 Vecteur de données numériques.
#' @param var2 Vecteur de données utilisé pour le croisement.
#' @param large Un booléen indiquant si des statistiques supplémentaires doivent être calculées.
#'
#' @return Un data frame contenant des statistiques descriptives croisées.
#'
#' Si `large` est TRUE, le tableau inclut les statistiques suivantes :
#'   - min, p01, p05, p20, p25, p40, p50, p60, p75, p80, p95, p99, max
#'   - mean : Moyenne
#'   - sd : Écart-type
#'   - n : Nombre total d'observations
#'   - NA(n) : Nombre d'observations manquantes
#'   - NA(%) : Pourcentage d'observations manquantes
#'
#' Si `large` est FALSE, le tableau inclut les statistiques suivantes :
#'   - min, p25, p50, p75, max
#'   - mean : Moyenne
#'   - sd : Écart-type
#'   - n : Nombre total d'observations
#'   - NA(n) : Nombre d'observations manquantes
#'
#' @export
distribCroisee <- function(var1, var2, large = FALSE) {
  # Le corps de la fonction reste inchangé
  if (large) {
    rep <- cbind("min" = tapply(var1, var2, stats::quantile, probs = 0.00, na.rm = T),
                 "p01" = tapply(var1, var2, stats::quantile, probs = 0.01, na.rm = T, type = 1),
                 "p05" = tapply(var1, var2, stats::quantile, probs = 0.05, na.rm = T, type = 1),
                 "p20" = tapply(var1, var2, stats::quantile, probs = 0.20, na.rm = T, type = 1),
                 "p25" = tapply(var1, var2, stats::quantile, probs = 0.25, na.rm = T, type = 1),
                 "p40" = tapply(var1, var2, stats::quantile, probs = 0.40, na.rm = T, type = 1),
                 "p50" = tapply(var1, var2, stats::quantile, probs = 0.50, na.rm = T, type = 1),
                 "p60" = tapply(var1, var2, stats::quantile, probs = 0.60, na.rm = T, type = 1),
                 "p75" = tapply(var1, var2, stats::quantile, probs = 0.75, na.rm = T, type = 1),
                 "p80" = tapply(var1, var2, stats::quantile, probs = 0.80, na.rm = T, type = 1),
                 "p95" = tapply(var1, var2, stats::quantile, probs = 0.95, na.rm = T, type = 1),
                 "p99" = tapply(var1, var2, stats::quantile, probs = 0.99, na.rm = T, type = 1),
                 "max" = tapply(var1, var2, stats::quantile, probs = 1.00, na.rm = T),
                 "mean"= tapply(var1, var2, base::mean, na.rm = T),
                 "sd"  = tapply(var1, var2, stats::sd, na.rm = T),
                 "n"     = tapply(var1, var2, function(var) {return(sum(is.na(var)) + sum(!is.na(var)))}),
                 "NA(n)" = tapply(var1, var2, function(var) {return(sum(is.na(var)))}),
                 "NA(%)" = tapply(var1, var2, function(var) {return(round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3))})
    )} else {
      rep <- cbind("min" = tapply(var1, var2, stats::quantile, probs = 0.00, na.rm = T),
                   "p25" = tapply(var1, var2, stats::quantile, probs = 0.25, na.rm = T, type = 1),
                   "p50" = tapply(var1, var2, stats::quantile, probs = 0.50, na.rm = T, type = 1),
                   "p75" = tapply(var1, var2, stats::quantile, probs = 0.75, na.rm = T, type = 1),
                   "max" = tapply(var1, var2, stats::quantile, probs = 1.00, na.rm = T),
                   "mean"= tapply(var1, var2, base::mean, na.rm = T),
                   "sd"  = tapply(var1, var2, stats::sd, na.rm = T),
                   "n"     = tapply(var1, var2, function(var) {return(sum(is.na(var)) + sum(!is.na(var)))}),
                   "NA(n)" = tapply(var1, var2, function(var) {return(sum(is.na(var)))})
      )}
  return(rep)
}
