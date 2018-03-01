#' S4-klasse die de totale bedekking van de soorten berekent
#' 
#' Deze klasse Bedekking staat in voor de berekening van waarden voor TypeVariabele Bedekking op basis van opgegeven kenmerken
#' 
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' 
#' @importFrom dplyr %>%
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
      ) %>%
      mutate(
        Gemiddelde =
          (.data$WaardeMin + .data$WaardeMax) / 2
      )

    #onderstaande is om te testen of het concept werkt, berekening moet nog aangepast worden!!!
    BedekkingGem <-
      (1 - prod( (100 - Resultaat$Gemiddelde) / 100, na.rm = TRUE)) * 100

    BedekkingMin <- BedekkingGem * 0.9
    BedekkingMax <- BedekkingGem * 1.1

    return(c(BedekkingMin, BedekkingMax))
  }
)
