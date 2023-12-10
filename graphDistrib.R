graphDistrib <- function(var) {
  data <- data.frame(value = var)
  plot <- ggplot(data = data, aes(x = value)) + geom_density()
  return(plot)
}
