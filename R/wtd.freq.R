#' Weighted frequency tables.
#'
#' Generate and format weighted frequency tables from a variable or a table, with percentages and formatting options.
#'
#' @param x either a vector to be tabulated, or a table object
#' @param weights vector of weights, must be the same length as `x`
#' @param digits number of digits to keep for the percentages
#' @param cum if TRUE, display cumulative percentages
#' @param total if TRUE, add a final row with totals
#' @param exclude vector of values to exclude from the tabulation (if `x` is a vector)
#' @param sort if specified, allow to sort the table by increasing ("inc") or decreasing ("dec") frequencies
#' @param valid if TRUE, display valid percentages
#' @param levels the desired levels for the factor in case of labelled vector (labelled package must be installed): "labels" for value labels, "values" for values or "prefixed" for labels prefixed with values
#' @param na.last if TRUE, NA values are always be last table row
#'
#' @return The result is an object of class data.frame.
#' @export
#'
#' @examples
#' sample <- data.frame(
#'  variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female", "Male"),
#'  weights =         c(1,        1,        3,  1,  2,      4,        1,    NA))
#' wtd.freq(sample$variable_x, sample$weights)

wtd.freq <- function(x, weights = NULL, digits = 1, cum = FALSE, total = FALSE,
                     exclude = NULL, sort = "", valid = !(NA %in% exclude),
                     levels = c("prefixed", "labels", "values"),
                     na.last = TRUE) {

  if (is.null(weights)) {
    warning("no weights argument given, using uniform weights of 1")
    weights <- rep(1, length(x))
  }

  levels <- match.arg(levels)
  if (is.table(x)) {
    tab <- x
  } else {
    if (!is.null(exclude)) {
      s <- !(x %in% exclude)
      x <- factor(x[s, drop = FALSE])
      weights <- weights[s]
    }
    tab_unwtd <- table(labelled::to_factor(x, levels), exclude = exclude)

    tab <- tapply(weights, labelled::to_factor(x, levels), sum, na.rm = T, simplify = TRUE)
    tab <- c(tab, "NA" = sum(weights[is.na(x)]))

    # tab <- as.table(tab_unwtd) * tab
  }


  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs/sum(effectifs, na.rm = TRUE) * 100)
  result <- data.frame(n_unwtd = tab_unwtd |> as.vector(),
                       n = effectifs,
                       pourc = pourc)
  if (valid) {
    user_na <- unique(as.character(labelled::to_factor(x,
                                                       levels)[is.na(x)]))
    NA.position <- length(tab)
    n.na <- sum(tab[NA.position], na.rm = TRUE)
    valid.pourc <- as.vector(effectifs/(sum(effectifs, na.rm = TRUE) -
                                          n.na) * 100)
    valid.pourc[NA.position] <- 0
    result <- cbind(result, valid.pourc)
  }
  if ("NA" %in% names(tab)) {
    names(tab)[names(tab) == "NA"] <- "\"NA\""
  }
  rownames(result) <- ifelse(is.na(names(tab)), "NA", names(tab))
  if (sort == "inc")
    result <- result[order(result$n), ]
  if (sort == "dec")
    result <- result[order(result$n, decreasing = TRUE),
    ]
  if (na.last && "NA" %in% rownames(result)) {
    result <- rbind(result[-which(rownames(result) == "NA"),
    ], result["NA", ])
  }
  if (total)
    result <- rbind(result, Total = apply(result, 2, sum, na.rm = TRUE))
  if (total & valid)
    result[length(result$pourc), "valid.pourc"] <- 100
  if (cum) {
    pourc.cum <- cumsum(result$pourc)
    if (total)
      pourc.cum[length(pourc.cum)] <- 100
    result <- cbind(result, pourc.cum)
    if (valid) {
      valid.pourc.cum <- cumsum(result$valid.pourc)
      if (total)
        valid.pourc.cum[length(valid.pourc.cum)] <- 100
      result <- cbind(result, valid.pourc.cum)
    }
  }
  if (valid) {
    NA.position <- which(rownames(result) == "NA" | rownames(result) %in%
                           user_na)
    result[NA.position, "valid.pourc"] <- NA
    if (cum)
      result[NA.position, "valid.pourc.cum"] <- NA
  }
  names(result)[names(result) == "pourc"] <- "%"
  names(result)[names(result) == "valid.pourc"] <- "val%"
  names(result)[names(result) == "pourc.cum"] <- "%cum"
  names(result)[names(result) == "valid.pourc.cum"] <- "val%cum"
  class(result) <- c("freqtab", class(result))
  round(result, digits = digits)
}
