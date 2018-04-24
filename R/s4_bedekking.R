#' S4-klasse die de totale bedekking van de soorten berekent
#' 
#' Deze klasse Bedekking staat in voor de berekening van waarden voor TypeVariabele Bedekking op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#' 
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' 
#' @importFrom methods setClass setMethod
#' 
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "bedekking",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "bedekking",
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

    # Resultaat <- Resultaat %>%
    #   mutate(
    #     Gemiddelde =
    #       (.data$WaardeMin + .data$WaardeMax) / 2
    #   )

    #onderstaande is om te testen of het concept werkt, berekening moet nog aangepast worden!!!
    BedekkingMin <-
      (1.0 - prod( (1.0 - Resultaat$WaardeMin), na.rm = TRUE))

    BedekkingMax <-
      (1.0 - prod( (1.0 - Resultaat$WaardeMax), na.rm = TRUE))

    # BedekkingMin <- BedekkingGem * 0.9
    # BedekkingMax <- BedekkingGem * 1.1

    return(c(BedekkingMin, BedekkingMax))
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "bedekking",
  definition = function(object) {
    return(100)
  }
)
