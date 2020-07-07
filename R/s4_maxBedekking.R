#' S4-klasse die de maximale bedekking van de soorten binnen een soortengroep
#' berekent
#'
#' Deze klasse MaxBedekking staat in voor de berekening van waarden voor
#' TypeVariabele Bedekking op basis van opgegeven kenmerken.  Ze is een
#' nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk,
#' TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#'
#' @noRd
#'
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "maxBedekking",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "maxBedekking",
  definition = function(object) {

    if (length(object@Kenmerken) > 0) {
      object@Kenmerken <- object@Kenmerken %>%
        filter(
          is.na(.data$Eenheid) |
            (!tolower(.data$Eenheid) %in% c("grondvlak_ha", "volume_ha"))
        )
    }

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
      MaxBedekkingMin <- NA
      MaxBedekkingMax <- NA
      warning("aan- of afwezigheid bedekking")
    } else if (nrow(Resultaat) > 0) {

      MaxBedekkingMin <- max(Resultaat$WaardeMin)
      MaxBedekkingMax <- max(Resultaat$WaardeMax)

    } else if (nrow(Resultaat) == 0) {

      MaxBedekkingMin <- 0
      MaxBedekkingMax <- 0

    }

    return(c(MaxBedekkingMin, MaxBedekkingMax))
  }
)
