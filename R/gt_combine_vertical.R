#' Vertically combine gt tables within LaTeX environment
#'
#' @param list_of_gt A list of gt tables already converted with "as_latex"
#'
#' @return
#' @export
#'
#' @examples

gt_combine_vertical <- function(list_of_gt) {
  if (length(list_of_gt) == 0) {
    cat("Empty list of tables.\n")
    return(NULL)
  }

  if (length(list_of_gt) == 1) {
    cat("List with one lonely table.\n")
    return(NULL)
  }

  list_of_gt <- sapply(list_of_gt, as.character)

  for (gt_table in seq_along(list_of_gt)[-length(list_of_gt)]) {
    list_of_gt[gt_table] <- list_of_gt[gt_table] |> str_split_i(pattern = fixed("\\bottomrule"), i = 1)
  }

  for (gt_table in seq_along(list_of_gt)[-length(list_of_gt)] + 1) {
    list_of_gt[gt_table] <- list_of_gt[gt_table] |> str_split_i(pattern = fixed("\\toprule"), i = 2)
  }

  result <- paste(list_of_gt, collapse = "\\midrule")

  return(result)
}

