#' Weighted Statistical Distribution Summary
#'
#' @param x A vector of numeric data.
#' @param y A vector of qualitative data (if you desire to compute statistics by groups).
#' @param w A numerical vector for weighting the distribution
#' @param filtering A logical vector used to filter `var` (and `var2`). Must be the same lenght.
#' @param probs The details wanted aside `mean` and `sd`
#' @param use_y_NA Include an NA level for `y`
#' @param include_n Include the number of observations used.
#'
#' @return
#' @export
#'
#' @examples
#'
#' sample <- data.frame(
#'   variable_x =      c(1,        2,        3, NA, NA,      4,        1,    2),
#'   variable_y = c("Male", "Female", "Female", NA, NA, "Male", "Female", "Male"),
#'      weights =      c(1,        1,        3,  1,  2,      4,        1,    NA))
#' wtd.distrib(x = sample$variable_x, y = sample$variable_y, w = sample$weights)
#' wtd.distrib(x = sample$variable_x, y = NULL, w = sample$weights)

wtd.distrib <- function(x, y = NULL, w, filtering = TRUE, probs = c(0.25*0:4), use_y_NA = FALSE, include_n = TRUE) {

  if (!is.null(y)) {

    x <- x[filtering]
    y <- y[filtering]
    w <- w[filtering]

    if (!is.numeric(x)) {stop("x has to be numeric.")}
    if (is.numeric(y)) {stop("y can't be numeric.")}
    levels_y <- c(unique(y) |> sort(), NA |> as.factor())
    wtd.distrib_ <- list()

    for (level in levels_y) {
      filt <- y %in% level

      if (all(is.na(x[filt]))) {
        level <- is.na(level) |> ifelse("NA", level)
        wtd.distrib_[[level]] <- rep(NA, length(probs) + 2) |> t() |>
          `colnames<-`(c(paste(format(round(probs * 100, if (length(probs) > 1) 2 - log10(diff(range(probs))) else 2)), "%", sep = ""), "mean", "sd")) |>
          as.data.frame()

      } else {
        level <- is.na(level) |> ifelse("NA", level)

        wtd.distrib_[[level]] <- Hmisc::wtd.quantile(x[filt], weights = w[filt], probs = probs) |> t()
        wtd.distrib_[[level]] <-
          cbind(wtd.distrib_[[level]],
                mean = Hmisc::wtd.mean(x[filt], weights = w[filt]),
                sd = Hmisc::wtd.var(x[filt], weights = w[filt]) |> sqrt()
          ) |> as.data.frame()
      }

      if (include_n) {
        wtd.distrib_[[level]] <-
          cbind(wtd.distrib_[[level]],
                n_valid = sum(!is.na(x[filt])),
                p_NA = sum(is.na(x[filt]))/sum(filt)
          )

        rm(level)
      }
    }

    wtd.distrib_ <- bind_rows(wtd.distrib_, .id = "level_")
    if (!use_y_NA) {wtd.distrib_ <- wtd.distrib_ |> filter(level_ != "NA")}
    wtd.distrib_ <- wtd.distrib_ |> column_to_rownames(var = "level_")

  } else {

    x <- x[filtering]
    y <- y[filtering]
    w <- w[filtering]

    wtd.distrib_ <- cbind(
      Hmisc::wtd.quantile(x, weights = w, probs = probs) |> t(),
      mean = Hmisc::wtd.mean(x, weights = w),
      sd = Hmisc::wtd.var(x, weights = w) |> sqrt()) |>
      as.data.frame()

    if (include_n) {
      wtd.distrib_ <- cbind(
        wtd.distrib_,
        n_NA = sum(is.na(x)),
        p_NA = sum(is.na(x))/length(x)
      )}

  }
  return(wtd.distrib_)
}
