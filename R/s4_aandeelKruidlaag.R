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
  Class = "aandeelKruidlaag",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "aandeelKruidlaag",
  definition = function(object) {

    Resultaat <- berekenWaarde(as(object, "bedekking"))/berekenWaarde(as(object, "bedekkingLaag")) * 100

    return(Resultaat)
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "aandeelKruidlaag",
  definition = function(object) {
    return(100)
  }
)
