#' Guillemets français
#' type is "in" or "out" depending if it is opening or closing.
#' @return
#' @export
#'
#' @examples
guillemetsFR <- function(type = "none") {

  if (type == "in") {return("« ")}
  if (type == "out") {return(" »")}
  message("Here comes the guillemets : · « ... » ·")}
