#' Compare Proportions by Country
#'
#' This function computes weighted tables of proportions for specified countries
#' and a combined set, optionally applying a filter to the data.
#'
#' @param x_vector A vector of the main variable to be analyzed.
#' @param y_vector A vector of the secondary variable to be analyzed.
#' @param w_vector A vector of weights. If NULL, equal weights are assumed.
#' @param country_vector A vector indicating the country for each observation.
#' @param countries A character vector of country codes to be included in the analysis.
#' @param y_index The index of the y dimension to use in the output table. Default is 1.
#' @param useNA How to handle NA values. Options are 'no', 'ifany', or 'always'. Default is 'no'.
#' @param filter A logical vector indicating which observations to include. If NULL, all observations are included.
#' @param fun_prop A function to calculate proportions. Default is `questionr::rprop`.
#' @param country_to An optional target format for country codes.
#' @param label_ensemble Label for the combined set of countries. Default is "Ensemble".
#'
#' @return A data frame containing the proportion tables for each specified country and the combined set.
#' @examples
#' # Example usage
#' x <- sample(c("Yes", "No"), 100, replace = TRUE)
#' y <- sample(c("A", "B"), 100, replace = TRUE)
#' w <- runif(100, 0.5, 1.5)
#' country <- sample(c("FR", "SE", "PT"), 100, replace = TRUE)
#' tbl_compa(x, y, w, country, countries = c("FR", "SE", "PT"))
#'
#' @export

tbl_compa <- function(
    x_vector,
    y_vector,
    w_vector = NULL,
    country_vector,
    countries,
    y_index = 1,
    useNA = 'no',
    filter = NULL,
    fun_prop = questionr::rprop,
    country_to = NULL,
    label_ensemble = ifelse(identical(questionr::rprop, fun_prop), "Total", "Ensemble")
) {

  # Input validation

  if (length(x_vector) != length(y_vector) || length(x_vector) != length(country_vector)) {
    stop("x_vector, y_vector, and country_vector must all have the same length.")
  }
  if (!is.null(filter) && length(filter) != length(x_vector)) {
    stop("Filter must have the same length as the input vectors.")
  }


  if (is.null(w_vector)) {w_vector <- rep(1, length(x_vector))}

  # Apply the filter

  if (is.null(filter)) {filter <- rep(TRUE, length(x_vector))}

  x_vector <- as.factor(x_vector[filter])
  y_vector <- y_vector[filter]
  w_vector <- w_vector[filter]
  country_vector <- country_vector[filter]


  # Convert country names if necessary

  if (!is.null(country_to)) {
    countries <- countries |>
      country_name("iso2", country_to) |>
      as.character()

    country_vector <- country_vector |>
      country_name("iso2", country_to) |>
      as.character()

  }



  tbl_list <- list()

  # Calculate tables for each country

  for (country in countries) {

    if (y_index %in% c(0, "all")) {



      table_country <-
        (questionr::wtd.table(
          x_vector[country_vector %in% country],
          y_vector[country_vector %in% country],
          w_vector[country_vector %in% country],
          useNA = useNA) |> fun_prop(drop = FALSE, n = FALSE))

      colnames(table_country) <- paste0(country, "_", colnames(table_country))

      tbl_list[[country]] <- table_country

    } else {
      tbl_list[[country]] <-
        (questionr::wtd.table(
          x_vector[country_vector %in% country],
          y_vector[country_vector %in% country],
          w_vector[country_vector %in% country],
          useNA = useNA) |> fun_prop(drop = FALSE, n = FALSE))[, y_index]}
  }



  # Calculate table for the entire set

  if (y_index %in% c(0, "all")) {

    tbl_list[[label_ensemble]] <-
      (questionr::wtd.table(
        x_vector,
        y_vector,
        w_vector,
        useNA = useNA) |> fun_prop(drop = FALSE, n = FALSE))

    colnames(tbl_list[[label_ensemble]]) <- paste0(label_ensemble, "_", colnames(tbl_list[[label_ensemble]]))


  } else {
    tbl_list[[label_ensemble]] <-
      (questionr::wtd.table(
        x_vector,
        y_vector,
        w_vector,
        useNA = useNA) |> fun_prop(drop = FALSE, n = FALSE))[, y_index]
  }

  # Create the final DataFrame

  tbl_df <-
    do.call(cbind, tbl_list) |>
    as.data.frame()

  # Add row names

  if (useNA != "no") {
    rownames(tbl_df) <- c(levels(droplevels(x_vector)), "NA", ifelse(identical(questionr::rprop, fun_prop), "Ensemble", "Total"))
  } else {
    rownames(tbl_df) <- c(levels(droplevels(x_vector)), ifelse(identical(questionr::rprop, fun_prop), "Ensemble", "Total"))
  }

  return(tbl_df)

}
