#' Formatting gt table(s) to LaTeX within a table environment
#'
#' @param x A (list of) gt table(s) already converted with "as_latex"
#' @param float Floating parameter for the table (defaults to "h")
#' @param tabcolsep Seperator between the colomns in pt (defaults to 0)
#' @param arraystretch Seperator between the rows in pt (defaults to 1)
#' @param stretchtable Setstretch parameter within the table
#' @param normalstretch Setstretch parameter outside the table
#' @param caption Caption of the table
#' @param caption_ Caption of the table (unnumbered)
#' @param use_longtable Using table environment and tabular
#'
#'
#' @return
#' @export
#'
#' @examples

gt_fmt_latex <- function(x, float = "h", tabcolsep = 0, arraystretch = 1, stretchtable = 1, normalstretch = 1.5, caption = NULL, caption_ = NULL, use_longtable = FALSE) {

  x <- gsub(
    x = x,
    pattern = "\n\\begin{minipage}",
    replacement = "\n\n\\vspace{3pt}\n\\begin{minipage}",
    fixed = TRUE)

  if (!use_longtable) {
    x <- gsub(
      x = x,
      pattern = "longtable",
      replacement = "tabular",
      fixed = TRUE)
  }

  paste0(
    ifelse(!use_longtable, paste0("\n\\begin{table}[", float, "]\n\\centering\n"), ""),
    "\n\\sffamily\n",
    "\n\\scriptsize\n\\setstretch{", stretchtable, "}\n",
    ifelse(!is.null(caption), paste0("\\caption{", caption, "}\n"), ""),
    ifelse(!is.null(caption_), paste0("\\caption{", caption_, "}\n"), ""),
    "\\setlength{\\tabcolsep}{", tabcolsep, "pt}\n", #  % Adjust horizontal padding
    "\\renewcommand{\\arraystretch}{", arraystretch, "}\n\n", #% Adjust vertical padding
    x,
    "\n\\end{table}\n")
}
