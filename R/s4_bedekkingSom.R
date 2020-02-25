#' S4-klasse die de bedekkingen van een soortengroep en studiegroep sommeert
#'
#' Deze klasse bedekkingSom staat in voor de berekening van waarden voor
#' AnalyseVariabele bedekkingSom op basis van opgegeven kenmerken.  Ze is een
#' nakomeling van de klasse bedekking.  Ze maakt de berekening als een som van
#' de bedekkingen van de studiegroep en de soortengroep.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod as
#'
#' @noRd
#'
#' @include s4_bedekking.R
setClass(
  Class = "bedekkingSom",
  representation =
    representation(),
  contains = "bedekking"
)

setMethod(
  f = "berekenWaarde",
  signature = "bedekkingSom",
  definition = function(object) {

    objectSoorten <- object
    objectSoorten@Studiegroep <- data.frame()
    bedekkingSoorten <- berekenWaarde(as(objectSoorten, "bedekking"))

    objectKenmerken <- object
    objectKenmerken@Soortengroep <- data.frame()
    bedekkingKenmerken <- berekenWaarde(as(objectKenmerken, "bedekking"))

    return(bedekkingSoorten + bedekkingKenmerken)
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "bedekkingSom",
  definition = function(object) {
    return(1)
  }
)
