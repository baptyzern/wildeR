#' Graphique de distribution croisée
#'
#' Cette fonction génère un graphique de distribution croisée pour une variable continue et une variable discrète.
#'
#' @param var1 Vecteur de données numériques pour l'axe x.
#' @param var2 Vecteur de données discrètes utilisé pour le croisement et la coloration.
#'
#' @return Un objet ggplot représentant le graphique de distribution croisée.
#'
#' @import ggplot2
#'
#' @examples
#' # Exemple d'utilisation de la fonction
#' data1 <- rnorm(100)
#' data2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' graphDistribCroisee(data1, data2)
#'
#' @export
graphDistribCroisee <- function(var1, var2) {
  data <- data.frame(value1 = var1, value2 = var2)
  plot <- ggplot(data = data, aes(x = value1, group = value2, color = value2)) +
    geom_density(linewidth = 0.7)
  return(plot)
}
