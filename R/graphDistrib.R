#' Graphique de distribution
#'
#' Cette fonction génère un graphique de distribution pour une variable donnée.
#'
#' @param var Vecteur de données numériques.
#'
#' @return Un objet ggplot représentant le graphique de distribution.
#'
#' @import ggplot2
#'
#' @examples
#' # Exemple d'utilisation de la fonction
#' data <- rnorm(100)
#' graphDistrib(data)
#'
#' @export
graphDistrib <- function(var) {
  data <- data.frame(value = var)
  plot <- ggplot(data = data, aes(x = value)) + geom_density()
  return(plot)
}
