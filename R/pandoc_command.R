#' Pandoc Command Generator
#'
#' @param input_file Path to the file to convert
#' @param input_format Format of the file to convert (if not given, then it is guessed)
#' @param output_format Format of the converted file (if not given, then it is guessed from the `output_file` extension)
#' @param output_file Path of the converted file (if not given, then it is guessed from `input_file` and `output_format`)
#'
#' @return The command that needs to be executed in a terminal (through the `shell` command when using Windows).
#'
#' @examples
#' # It is possible to force the file extension of the output.
#' # Pandoc only knows about markdown, but qmd file use the same syntax as markdown
#' # so there is no issue when outputting to a qmd file.
#' # pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd")
#' # shell(pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd"))
#'
#' @export

pandoc_command <- function(input_file, input_format = NULL, output_format = NULL, output_file = NULL) {

  if (is.null(output_format) & is.null(output_file)) {stop("No output format nor output file was provided.")}

  input_format_extension <- tools::file_ext(input_file)
  file_raw_name <- substr(input_file, start = 1, stop = nchar(input_file) - 1 - nchar(input_format_extension))

  input_format_command <- ifelse(is.null(input_format), input_format_extension, input_format)
  input_format_command <- ifelse(input_format_command == "md", "markdown", input_format_command)
  input_format_command <- ifelse(input_format_command == "tex", "latex", input_format_command)
  input_format_command <- ifelse(input_format_command == "md", "markdown", input_format_command)



  if (is.null(output_format)) {
    output_format <- tools::file_ext(output_file)
  }

  output_format_command <- output_format
  output_format_command <- ifelse(output_format_command == "md", "markdown", output_format_command)
  output_format_command <- ifelse(output_format_command == "tex", "latex", output_format_command)
  output_format_command <- ifelse(output_format_command == "md", "markdown", output_format_command)

  output_format_extension <- output_format
  output_format_extension <- ifelse(output_format_extension == "markdown", "md", output_format_extension)
  output_format_extension <- ifelse(output_format_extension == "latex", "tex", output_format_extension)
  output_format_extension <- ifelse(output_format_extension == "word", "docx", output_format_extension)



  if (is.null(output_file)) {
    output_file <- paste0(file_raw_name, ".", output_format_extension)
  }


  # Exemple pandoc test1.md -f markdown -t latex -s -o test1.tex

  shell_command <- paste0(
    "pandoc ",
    input_file,
    " -f ",
    input_format_command,
    " -t ",
    output_format_command,
    " -s -o ",
    output_file)

  return(shell_command)
}
