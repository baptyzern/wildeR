#' Formatting gt table(s) to LaTeX within a table environment
#'
#' @param x A (list of) gt table(s) already converted with "as_latex"
#' @param float Floating parameter for the table (defaults to "h")
#' @param tabcolsep Seperator between the colomns in pt (defaults to 0)
#' @param arraystretch Seperator between the rows in pt (defaults to 1)
#' @param stretchtable Setstretch parameter within the table
#' @param normalstretch Setstretch parameter outside the table
#'
#' @return
#' @export
#'
#' @examples

gt_fmt_latex <- function(x, float = "h", tabcolsep = 0, arraystretch = 1, stretchtable = 1, normalstretch = 1.5) {

  x <- gsub(
    x = x,
    pattern = "\n\\begin{minipage}",
    replacement = "\n\\vspace{4pt}\n\\begin{minipage}",
    fixed = TRUE)

  paste0("\n\\begin{table}[", float, "]\n\n\\scriptsize\n\\setstretch{", stretchtable, "}\n",
         "\\setlength{\\tabcolsep}{", tabcolsep, "pt} % Adjust horizontal padding\n",
         "\\renewcommand{\\arraystretch}{", arraystretch, "} % Adjust vertical padding\n\n",
         x,
         "\n\\end{table}\n\n\\normalsize\n\\setstretch{", normalstretch, "}\n")
}
