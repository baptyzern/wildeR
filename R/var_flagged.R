#' Combine a character or factor variable with its flags
#'
#' @param x Main variable
#' @param x_flag Flag variable attached to main variable
#' @param flag_meaning Flag description under the form of named (numerical) vector (see examples)
#'
#' @return Factor vector
#' @export
#'
#' @examples
#' sample <- data.frame(variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female"),
#'                      variable_x_F = c(1, 1, 1, -1, -2, 1, 1))
#' flag_meaning <- c("Missing" = -1, "Not applicable" = -2)
#' var_flagged(x = sample$variable_x, x_flag = sample$variable_x_F,
#'             flag_meaning = c("Missing" = -1, "Not applicable" = -2))

var_flagged <- function(x, x_flag, flag_meaning) {

  if (is.factor(x)) {levels_x <- levels(x)} else {levels_x <- unique(x)[!is.na(unique(x))]}

  flag_df <- data.frame(value = flag_meaning, name = paste0("F_", names(flag_meaning)))

  flags <- data.frame(x, x_flag)
  flags$labelled_flag <- factor(flags$x_flag, levels = flag_df$value, labels = flag_df$name) |> as.character()

  res <- ifelse(is.na(flags$x), flags$labelled_flag, as.character(flags$x))
  res <- factor(res, levels = c(levels_x, paste0("F_", names(flag_meaning))))

  return(res)
}
