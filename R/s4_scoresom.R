#' S4-klasse die de som van de toegekende klasses berekent
#'
#' Deze klasse Scoresom staat in voor de berekening van waarden voor
#' TypeVariabele Scoresom op basis van opgegeven kenmerken.  Ze is een
#' nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#' @importFrom dplyr %>% filter
#'
#' @noRd
#'
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "scoresom",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "scoresom",
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

    #Als er meer NA's zijn in WaardeMax dan in WaardeMin, wil dit zeggen dat
    #er aan-/afwezigheden opgegeven zijn in plaats van bedekkingen
    #In dat geval berekenen we geen bedekking en geven we een warning
    if (sum(is.na(Resultaat$WaardeMin)) < sum(is.na(Resultaat$WaardeMax))) {
      ScoresomMin <- NA
      ScoresomMax <- NA
      warning("aan- of afwezigheid bedekking")
    } else {
      ScoresomMin <- sum(Resultaat$WaardeMin, na.rm = TRUE)
      ScoresomMax <- sum(Resultaat$WaardeMax, na.rm = TRUE)
    }

    return(c(ScoresomMin, ScoresomMax))
  }
)
