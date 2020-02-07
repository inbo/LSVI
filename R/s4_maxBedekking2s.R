#' S4-klasse die de totale bedekking van de 2 soorten met hoogste bedekking
#' binnen een soortengroep berekent
#'
#' Deze klasse MaxBedekking2s staat in voor de berekening van waarden voor
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
  Class = "maxBedekking2s",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "maxBedekking2s",
  definition = function(object) {

    object@Kenmerken <- object@Kenmerken %>%
      filter(
        is.na(.data$Eenheid) |
          (!tolower(.data$Eenheid) %in% c("grondvlak_ha", "volume_ha"))
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
      MaxBedekkingMin <- NA
      MaxBedekkingMax <- NA
      warning("aan- of afwezigheid bedekking")
    } else if (nrow(Resultaat) > 1) {

      MaxBedekking2sMin <-
        1.0 - prod((1.0 - sort(Resultaat$WaardeMin, decreasing = TRUE)[1:2]))
      MaxBedekking2sMax <-
        1.0 - prod((1.0 - sort(Resultaat$WaardeMax, decreasing = TRUE)[1:2]))

    } else if (nrow(Resultaat) == 1) {

      MaxBedekking2sMin <- Resultaat$WaardeMin
      MaxBedekking2sMax <- Resultaat$WaardeMax

    } else if (nrow(Resultaat) == 0) {

      MaxBedekking2sMin <- 0
      MaxBedekking2sMax <- 0

    }

    return(c(MaxBedekking2sMin, MaxBedekking2sMax))
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "maxBedekking2s",
  definition = function(object) {
    return(1)
  }
)
