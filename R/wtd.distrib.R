#' Title
#'
#' @param x
#' @param y
#' @param w
#' @param filtering
#'
#' @return
#' @export
#'
#' sample <- data.frame(
#'  variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female", "Male"),
#'  variable_y =      c(1,        2,        3, NA, NA,      4,        1,    2),
#'  weights =         c(1,        1,        3,  1,  2,      4,        1,    NA))


# sample <- data.frame(
#  variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female", "Male"),
#  variable_y =      c(1,        2,        3, NA, NA,      4,        1,    2),
#  weights =         c(1,        1,        3,  1,  2,      4,        1,    NA))
# x = sample$variable_y
# y = sample$variable_x
# w = sample$weights

wtd.distrib <- function(x, y = TRUE, w, filtering = TRUE, probs = c(0.1*0:10)) {

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

      rm(level)

    } else {
      level <- is.na(level) |> ifelse("NA", level)

      wtd.distrib_[[level]] <- Hmisc::wtd.quantile(x[filt], weights = w[filt], probs = probs) |> t()
      wtd.distrib_[[level]] <-
        cbind(wtd.distrib_[[level]],
              mean = Hmisc::wtd.mean(x[filt], weights = w[filt]),
              sd = Hmisc::wtd.var(x[filt], weights = w[filt]) |> sqrt()) |> as.data.frame()
      rm(level)}
  }

  wtd.distrib_ <- bind_rows(wtd.distrib_, .id = "level_")
  wtd.distrib_ <- wtd.distrib_ |> `rownames<-`(wtd.distrib_$level_) |> select(-level_)
  return(wtd.distrib_)
}
