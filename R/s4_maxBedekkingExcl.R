#' S4-klasse die de maximale bedekking berekent van de soorten die niet tot de soortengroep behoren
#'
#' Deze klasse MaxBedekkingExcl staat in voor de berekening van waarden voor TypeVariabele Bedekking op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#'
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "maxBedekkingExcl",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "maxBedekkingExcl",
  definition = function(object) {

    object@Kenmerken <- object@Kenmerken %>%
      filter(is.na(.data$Eenheid) | (!.data$Eenheid %in% c("Grondvlak_ha", "Volume_ha")))

    Resultaat <-
      deselecteerKenmerkenInOpname(
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
      MaxBedekkingMin <- NA
      MaxBedekkingMax <- NA
      warning("De bedekking is niet berekend voor indicatoren waarbij voor sommige soorten of kenmerken enkel aan- of afwezigheid opgegeven is") #nolint
    } else if (nrow(Resultaat) > 0){

      MaxBedekkingMin <- max(Resultaat$WaardeMin)
      MaxBedekkingMax <- max(Resultaat$WaardeMax)

    } else if (nrow(Resultaat) == 0){

      MaxBedekkingMin <- 0
      MaxBedekkingMax <- 0

    }

    return(c(MaxBedekkingMin, MaxBedekkingMax))
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "maxBedekkingExcl",
  definition = function(object) {
    return(1)
  }
)
