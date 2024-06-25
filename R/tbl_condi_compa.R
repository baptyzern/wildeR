#' Compare Proportions by Country using conditions
#'
#' @param x_vector A vector of the main variable to be analyzed.
#' @param w_vector A vector of weights. If NULL, equal weights are assumed.
#' @param country_vector A vector indicating the country for each observation.
#' @param countries A character vector of country codes to be included in the analysis.
#' @param x_index The index of the y dimension to use in the output table. Default is 1. 0 means you keep all
#' @param useNA How to handle NA values. Options are 'no', 'ifany', or 'always'. Default is 'always'.
#' @param filter A logical vector indicating which observations to include. If NULL, all observations are included.
#' @param fun_prop A function to calculate proportions. Default is `questionr::rprop`.
#' @param country_to
#' @param label_ensemble
#' @param conditions A named list of logical vectors indicating the conditions for each subset.
#'
#' @return
#' @export
#'
#' @examples
#' data("hdv2003", package = "questionr")
#' tbl_condi_compa(
#'   x_vector = hdv2003$hard.rock,
#'   w_vector = hdv2003$poids,
#'   country_vector = hdv2003$sexe,
#'   countries = c("Homme", "Femme"),
#'   x_index = 0,
#'   conditions = list("Fait du sport" = hdv2003$sport %in% "Oui")
#'   )

tbl_condi_compa <- function(
    x_vector,
    w_vector = NULL,
    country_vector,
    countries,
    x_index = 1,
    useNA = 'no',
    filter = NULL,
    fun_prop = questionr::rprop,
    drop_funprop = FALSE,
    country_to = NULL,
    label_ensemble = ifelse(identical(questionr::rprop, fun_prop), "Total", "Ensemble"),
    conditions = list(named = TRUE)
) {

  # Input validation
  if (!is.list(conditions) || length(conditions) == 0 || !all(vapply(conditions, is.logical, logical(1)))) {
    stop("conditions must be a named list of logical vectors.")
  }
  if (!is.null(w_vector) && length(w_vector) != length(x_vector)) {
    stop("w_vector must be NULL or the same length as x_vector.")
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
      tbl_compa(
        x_vector = conditions[[condition]],
        y_vector = x_vector,
        w_vector,
        country_vector,
        countries,
        y_index = x_index,
        useNA,
        filter,
        fun_prop = questionr::rprop,
        country_to,
        label_ensemble
      )

    tbl_list[[condition]] <- tbl_list[[condition]][match(TRUE, row.names(tbl_list[[condition]])), ]
    }

  result <- tbl_list |>
    dplyr::bind_rows(.id = "condition") |>
    tibble::remove_rownames() |>
    tibble::column_to_rownames("condition")

  return(result)





}
