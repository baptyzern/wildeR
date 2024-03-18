#' Conversion entre nom complet de pays et leur code ISO2
#'
#' @param list Vecteur de classe caractère à convertir
#' @param from Point de départ de la conversion (name_fr, name_en, name_de, iso2, iso3)
#' @param to Point d'arrivée de la conversion (name_fr, name_en, name_de, iso2, iso3)
#' @param drop_levels Enlever les pays en trop ? par défaut, TRUE
#'
#' @export


country_name <- function(list, from = c("name_fr", "name_en", "name_de", "iso2", "iso3"), to = c("name_fr", "name_en", "name_de", "iso2", "iso3"), drop_levels = TRUE) {

  if (from == "iso2") {
    if (to == "name_fr") {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO2_CODE, labels = wildeR::world_seen_by_eu$NAME_FREN)}
    if (to == "name_en") {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO2_CODE, labels = wildeR::world_seen_by_eu$NAME_ENGL)}
    if (to == "name_de") {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO2_CODE, labels = wildeR::world_seen_by_eu$NAME_GERM)}
    if (to == "iso3")    {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO2_CODE, labels = wildeR::world_seen_by_eu$ISO3_CODE)}
  }

  if (from == "iso3") {
    if (to == "name_fr") {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO3_CODE, labels = wildeR::world_seen_by_eu$NAME_FREN)}
    if (to == "name_en") {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO3_CODE, labels = wildeR::world_seen_by_eu$NAME_ENGL)}
    if (to == "name_de") {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO3_CODE, labels = wildeR::world_seen_by_eu$NAME_GERM)}
    if (to == "iso2")    {list <- factor(list, levels = wildeR::world_seen_by_eu$ISO3_CODE, labels = wildeR::world_seen_by_eu$ISO2_CODE)}
  }

  if (from == "name_fr") {
    if (to == "name_en") {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_FREN, labels = wildeR::world_seen_by_eu$NAME_ENGL)}
    if (to == "name_de") {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_FREN, labels = wildeR::world_seen_by_eu$NAME_GERM)}
    if (to == "iso2")    {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_FREN, labels = wildeR::world_seen_by_eu$ISO2_CODE)}
    if (to == "iso3")    {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_FREN, labels = wildeR::world_seen_by_eu$ISO3_CODE)}
  }

  if (from == "name_en") {
    if (to == "name_fr") {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_ENGL, labels = wildeR::world_seen_by_eu$NAME_FREN)}
    if (to == "name_de") {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_ENGL, labels = wildeR::world_seen_by_eu$NAME_GERM)}
    if (to == "iso2")    {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_ENGL, labels = wildeR::world_seen_by_eu$ISO2_CODE)}
    if (to == "iso2")    {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_ENGL, labels = wildeR::world_seen_by_eu$ISO3_CODE)}
  }

  if (from == "name_de") {
    if (to == "name_fr") {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_GERM, labels = wildeR::world_seen_by_eu$NAME_FREN)}
    if (to == "name_en") {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_GERM, labels = wildeR::world_seen_by_eu$NAME_ENGL)}
    if (to == "iso2")    {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_GERM, labels = wildeR::world_seen_by_eu$ISO2_CODE)}
    if (to == "iso2")    {list <- factor(list, levels = wildeR::world_seen_by_eu$NAME_GERM, labels = wildeR::world_seen_by_eu$ISO3_CODE)}
  }

  if(drop_levels) {list <- droplevels.factor(list)}

  return(list)
}
