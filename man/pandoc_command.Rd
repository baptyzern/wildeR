% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pandoc_command.R
\name{pandoc_command}
\alias{pandoc_command}
\title{Pandoc Command Generator}
\usage{
pandoc_command(
  input_file,
  input_format = NULL,
  output_format = NULL,
  output_file = NULL
)
}
\arguments{
\item{input_file}{Path to the file to convert}

\item{input_format}{Format of the file to convert (if not given, then it is guessed)}

\item{output_format}{Format of the converted file (if not given, then it is guessed from the \code{output_file} extension)}

\item{output_file}{Path of the converted file (if not given, then it is guessed from \code{input_file} and \code{output_format})}
}
\value{
The command that needs to be executed in a terminal (through the \code{system} R command, for example).
}
\description{
Pandoc Command Generator
}
\examples{
# It is possible to force the file extension of the output.
# Pandoc only knows about markdown, but qmd file use the same syntax as markdown
# so there is no issue when outputting to a qmd file.
# pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd")

# You can combine the pandoc_command call with the system() function to send the output directly to the terminal.
# system(pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd"))
# pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd") |> system()
}
