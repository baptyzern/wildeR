graphDistribCroisee <- function(var1, var2) {
  data <- data.frame(value1 = var1, value2 = var2)
  plot <- ggplot(data = data, aes(x = value1, group = value2, color = value2)) +
    geom_density()
  return(plot)
}
