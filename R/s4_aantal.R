#' S4-klasse die aantal soorten met een bepaalde bedekking berekent
#' 
#' Deze klasse Aantal staat in voor de berekening van waarden voor TypeVariabele Aantal op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#' 
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' 
#' @importFrom dplyr %>%
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "aantal",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "aantal",
  definition = function(object) {

    Resultaat <-
      selecteerKenmerkenInOpname(
        object@Kenmerken,
        object@Soortengroep,
        object@Studiegroep,
        object@SubAnalyseVariabele,
        object@SubRefMin,
        object@SubRefMax,
        object@SubOperator
      )

    if (length(Resultaat) == 1 & all(is.na(Resultaat))) {
      return(NA)
    }
    
    Aantal <- nrow(Resultaat)

    return(Aantal)
  }
)
