#' S4-klasse die aandeel grondvlak of volume van bepaalde boomsoort berekend
#'
#' Deze klasse Aandeel staat in voor de berekening van waarden voor TypeVariabele Aandeel op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#'
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "aandeel",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "aandeel",
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

     Totaal <- sum(mean(c(Resultaat$WaardeMin, Resultaat$WaardeMax)))

    return(Totaal)
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "aandeel",
  definition = function(object) {
    return(100)
  }
)
