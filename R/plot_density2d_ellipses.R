#' Title
#'
#' @param x
#' @param y
#' @param groups
#' @param ellipse_levels
#' @param weights
#'
#' @return
#' @export
#'
#' @examples

plot_density2d_ellipses <- function(x, y, groups = NULL, ellipse_levels = 0.5, weights) {

  kde2d_out <- ggtern::kde2d.weighted(
    x = x,
    y = y,
    # h = 0.1,
    # n = 50,
    w = weights,
    lims = c(-3, 3, -3, 3)
  )

  nbreaks_ <- max(kde2d_out$z) %/% 0.02

  filled.contour(
    kde2d_out,
    col = c("#00000000", viridis(nbreaks_, option = "F", direction = -1)),
    nlevels = nbreaks_ + 1,
    xlab = var_label(x),
    ylab = var_label(y)
  )

  if (!is.null(groups)) {
    car::dataEllipse(
      x = cbind(x, y),
      groups = groups,
      plot.points = F,
      levels = ellipse_levels,
      col = RColorBrewer::brewer.pal(nlevels(as.factor(groups)), "Set1"),
      add = T,
      weights = weights
    )
  }
}
