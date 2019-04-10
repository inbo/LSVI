#' S4-klasse die de totale bedekking van de soorten berekent
#'
#' Deze klasse Bedekking staat in voor de berekening van waarden voor TypeVariabele Bedekking op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
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


    object@Kenmerken <- object@Kenmerken %>%
      filter(
        is.na(.data$Eenheid) |
          (!.data$Eenheid %in% c("Grondvlak_ha", "Volume_ha"))
      )

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
      BedekkingMin <- NA
      BedekkingMax <- NA
      warning("aan- of afwezigheid bedekking")
    } else {
      BedekkingMin <-
        (1.0 - prod( (1.0 - Resultaat$WaardeMin), na.rm = TRUE))
      BedekkingMax <-
        (1.0 - prod( (1.0 - Resultaat$WaardeMax), na.rm = TRUE))
    }

    return(c(BedekkingMin, BedekkingMax))
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "bedekking",
  definition = function(object) {
    return(1)
  }
)
