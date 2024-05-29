#' Conditional Weighted Frequency Table
#'
#' This function computes conditional weighted frequency tables for specified conditions.
#'
#' @param x_vector A vector of the main variable to be analyzed.
#' @param conditions A named list of logical vectors indicating the conditions for each subset.
#' @param w_vector A vector of weights. If NULL, equal weights are assumed.
#' @param useNA How to handle NA values. Options are 'no', 'ifany', or 'always'. Default is 'always'.
#' @param total Logical. Should the total be included in the output? Default is TRUE.
#' @param digits Number of decimal places to include in the results. Default is 1.
#'
#' @return A data frame with the frequency tables for each condition and the total.
#' @examples
#' # Example usage
#' x <- sample(c("Yes", "No"), 100, replace = TRUE)
#' y <- sample(c("Male", "Female"), 100, replace = TRUE)
#' z <- sample(c("Worker", "Inactive"), 100, replace = TRUE)
#' w <- runif(100, 0.5, 1.5)
#' df <- data.frame(x, y, z, w)
#' cond <- list(
#'   "Male" = df$y %in% "Male",
#'   "Male Worker" = df$y %in% "Male" & df$z %in% "Worker",
#'   "Female" = df$y %in% "Female",
#' "Female Inactive" = df$y %in% "Female" & df$z %in% "Inactive"
#' )
#' result <- tbl_conditional(df$x, cond, df$w)
#' print(result)
#'
#' @export

tbl_conditional <- function(
    x_vector,
    conditions = list(named = TRUE),
    w_vector = NULL,
    useNA = "no",
    total = TRUE,
    digits = 1
) {

  # Input validation
  if (!is.list(conditions) || length(conditions) == 0 || !all(vapply(conditions, is.logical, logical(1)))) {
    stop("conditions must be a named list of logical vectors.")
  }
  if (!is.null(w_vector) && length(w_vector) != length(x_vector)) {
    stop("w_vector must be NULL or the same length as x_vector.")
  }
  if (!is.logical(total) || length(total) != 1) {
    stop("total must be a single logical value.")
  }


  # Giving equal weights if w_vector doesn't exist
  if (is.null(w_vector)) {
    w_vector <- rep(1, length(x_vector))
  }

  tbl_list <- list()

    for (condition in names(conditions)) {
    if (length(conditions[[condition]]) != length(x_vector)) {
      stop(paste("Condition", condition, "must have the same length as x_vector."))
    }

    tbl_list[[condition]] <-
      questionr::wtd.table(
        x = x_vector[conditions[[condition]]],
        weights = w_vector[conditions[[condition]]],
        useNA = useNA
      ) |>
      questionr::freq(valid = FALSE, total = total, digits = digits) |>
      dplyr::pull("%")
  }

  if (useNA != "no") {
    names_tbl <- x_vector |>
      factor() |>
      forcats::fct_na_value_to_level() |>
      levels()
  } else {
    names_tbl <- x_vector |>
      factor() |>
      levels()
  }

  if (total) {
    names_tbl <- c(names_tbl, "Total")
  }

  result <- tbl_list |>
    dplyr::bind_rows() |>
    t() |>
    `colnames<-`(names_tbl)

  return(result)
}
