#' Correction of rgba calls after as_latex
#'
#' @param A character vector of a gt LaTeX table
#' @param df_colors data.frama with rgba calls and HTML color tag
#'
#' @return
#' @export
#'
#' @examples

gt_color_latex <- function (x, df_colors = tibble::tribble(~rgb, ~html,
                                       "rgba(0,255,0,0.2)",     "#E0FFE0",
                                       "rgba(255,255,0,0.2)",   "#FFFFE0",
                                       "rgba(0,0,139,0.2)",     "#CCCCE8",
                                       "rgba(0,0,139,0.15)",    "#D9D9EE",)
                            ) {

  df_colors$html <- sub(pattern = "#", replacement = "", df_colors$html)

  for (i in 1:nrow(df_colors)) {
    x <- gsub(
      x = x,
      pattern = df_colors$rgb[i],
      replacement = df_colors$html[i],
      fixed = TRUE)}

  return(x)

}
