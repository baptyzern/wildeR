#' Distribution de statistiques descriptives
#'
#' Cette fonction calcule différentes statistiques descriptives en fonction de l'argument `large`.
#'
#' @param var Vecteur de données numériques.
#' @param large Un booléen indiquant si des statistiques supplémentaires doivent être calculées.
#'
#' @return Un data frame contenant des statistiques descriptives.
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
distrib <- function(var, large = FALSE) {
  # Le corps de la fonction reste inchangé
  if (large) {
    rep <- cbind("min" = stats::quantile(var, probs = 0.00, na.rm = T),
                 "p01" = stats::quantile(var, probs = 0.01, na.rm = T, type = 1),
                 "p05" = stats::quantile(var, probs = 0.05, na.rm = T, type = 1),
                 "p20" = stats::quantile(var, probs = 0.20, na.rm = T, type = 1),
                 "p25" = stats::quantile(var, probs = 0.25, na.rm = T, type = 1),
                 "p40" = stats::quantile(var, probs = 0.40, na.rm = T, type = 1),
                 "p50" = stats::quantile(var, probs = 0.50, na.rm = T, type = 1),
                 "p60" = stats::quantile(var, probs = 0.60, na.rm = T, type = 1),
                 "p75" = stats::quantile(var, probs = 0.75, na.rm = T, type = 1),
                 "p80" = stats::quantile(var, probs = 0.80, na.rm = T, type = 1),
                 "p95" = stats::quantile(var, probs = 0.95, na.rm = T, type = 1),
                 "p99" = stats::quantile(var, probs = 0.99, na.rm = T, type = 1),
                 "max" = stats::quantile(var, probs = 1.00, na.rm = T),
                 "mean"= base::mean(var, na.rm = T),
                 "sd"  = stats::sd(var, na.rm = T),
                 "n"     = sum(is.na(var)) + sum(!is.na(var)),
                 "NA(n)" = sum(is.na(var)),
                 "NA(%)" = round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3)
    )} else {
      rep <- cbind("min" = stats::quantile(var, probs = 0.00, na.rm = T),
                   "p25" = stats::quantile(var, probs = 0.25, na.rm = T, type = 1),
                   "p50" = stats::quantile(var, probs = 0.50, na.rm = T, type = 1),
                   "p75" = stats::quantile(var, probs = 0.75, na.rm = T, type = 1),
                   "max" = stats::quantile(var, probs = 1.00, na.rm = T),
                   "mean"= base::mean(var, na.rm = T),
                   "sd"  = stats::sd(var, na.rm = T),
                   "n"     = sum(is.na(var)) + sum(!is.na(var)),
                   "NA(n)" = sum(is.na(var))
      )}
  rownames(rep) <- NULL
  return(rep)
}
