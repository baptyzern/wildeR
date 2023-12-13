#' Conversion entre nom complet de pays et leur code ISO2
#'
#' @param list Vecteur de classe caractère à convertir
#' @param from Point de départ de la conversion (name, ou iso2)
#' @param lang Langue du nom complet
#'
#' @export


country_name <- function(list, from = c("name", "iso2"), lang = "fr") {
  if (from == "iso2") {
    if (lang == "fr") {list <- factor(list, levels = wildeR::EUcountries$abreviation, labels = wildeR::EUcountries$FRcountry)}
    if (lang == "en") {list <- factor(list, levels = wildeR::EUcountries$abreviation, labels = wildeR::EUcountries$ENcountry)}
  }

  if (from == "name") {
    if (lang == "fr") {list <- factor(list, levels = wildeR::EUcountries$FRcountry, labels = wildeR::EUcountries$abreviation)}
    if (lang == "en") {list <- factor(list, levels = wildeR::EUcountries$ENcountry, labels = wildeR::EUcountries$abreviation)}
  }

  return(list)
}
