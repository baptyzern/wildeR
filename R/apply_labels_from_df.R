#' Apply labels to a data frame using another data frame
#'
#' @param df "Data frame to which labels will be applied"
#' @param df_with_labels "Data frame where labels are supplied"
#' @param col_name "Name of the column in df_with_labels where columns of df are supplied"
#' @param col_label "Name of the column in df_with_labels where the corresponding labels are supplied"
#'
#' @return A nice and pretty data frame with column labels
#' @export
#'
#' @import labelled
#'
#' @examples
#' tt <- apply_labels_from_df(world_seen_by_eu[, -12], variables_world_seen_by_eu, col_label = "VariableLabel_FR")
#' tt
#' labelled::var_label(tt)

apply_labels_from_df <- function(df, df_with_labels, col_name = "VariableName", col_label = "VariableLabel") {

  cols <- colnames(df)
  available <- sapply(df, function(x) !all(is.na(x)))
  dataCols <- data.frame(cols, available)
  labels <- merge(df_with_labels, dataCols, all.x = T, all.y = T, by.y = "cols", by.x = col_name)
  labels <- labels[match(cols, labels[, col_name]),]
  df <- labelled::set_variable_labels(.data = df, .labels = labels[, col_label])

  return(df)
}
