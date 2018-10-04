#' S4-klasse die aandeelVegetatielaag bedekking soorten binnen een vegetatielaag berekend
#'
#' Deze klasse AandeelVegetatielaag staat in voor de berekening van waarden voor TypeVariabele AandeelVegetatielaag op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#'
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "aandeelVegetatielaag",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "aandeelVegetatielaag",
  definition = function(object) {

    Resultaat <- berekenWaarde(as(object, "bedekking"))/berekenWaarde(as(object, "bedekkingLaag"))

    return(Resultaat)
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "aandeelVegetatielaag",
  definition = function(object) {
    return(100)
  }
)
