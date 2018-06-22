#' S4-klasse die aantal soorten met een bepaalde bedekking berekent
#'
#' Deze klasse Aantal staat in voor de berekening van waarden voor TypeVariabele Aantal op basis van opgegeven kenmerken.  Ze is een nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#' @importFrom dplyr %>% mutate row_number filter
#'
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

    Kenmerken <- object@Kenmerken
    #Als er meer NA's zijn in WaardeMax dan in WaardeMin, wil dit zeggen dat
    #er aan-/afwezigheden opgegeven zijn in plaats van bedekkingen
    #Als er in dat geval een subvoorwaarde opgegeven is, zijn we niet
    #zeker of aan de subvoorwaarde voldaan is, dus we berekenen beide opties
    #en geven een warning als de opties niet hetzelfde resultaat opleveren
    if (
      (sum(is.na(Kenmerken$WaardeMin)) < sum(is.na(Kenmerken$WaardeMax))) &
      !identical(object@SubAnalyseVariabele, character(0))
    ) {
      Kenmerken <-
        Kenmerken %>%
        mutate(
          WaardeMax =
            ifelse(
              is.na(.data$WaardeMax) & .data$WaardeMin == 0,
              0,
              .data$WaardeMax
            )
        )
      Problemen <-
        (Kenmerken %>%
           mutate(
             Rijnummers = row_number(.data$ID)
           ) %>%
           filter(
             is.na(.data$WaardeMax) & .data$WaardeMin == 1
           )
        )$Rijnummers

      KenmerkenMax <- Kenmerken
      KenmerkenMax[Problemen, ]$WaardeMin <- 1
      KenmerkenMax[Problemen, ]$WaardeMax <- 1
      ResultaatMax <-
        selecteerKenmerkenInOpname(
          KenmerkenMax,
          object@Soortengroep,
          object@Studiegroep,
          object@SubAnalyseVariabele,
          object@SubRefMin,
          object@SubRefMax,
          object@SubOperator
        )
      if (length(ResultaatMax) == 1 & all(is.na(ResultaatMax))) {
        return(NA)
      }
      AantalMax <- nrow(ResultaatMax)

      KenmerkenMin <- Kenmerken
      KenmerkenMin[Problemen, ]$WaardeMin <- 0
      KenmerkenMin[Problemen, ]$WaardeMax <- 0
      ResultaatMin <-
        selecteerKenmerkenInOpname(
          KenmerkenMin,
          object@Soortengroep,
          object@Studiegroep,
          object@SubAnalyseVariabele,
          object@SubRefMin,
          object@SubRefMax,
          object@SubOperator
        )
      if (length(ResultaatMin) == 1 & all(is.na(ResultaatMin))) {
        AantalMin <- 0
      }
      AantalMin <- nrow(ResultaatMin)

      Aantal <- c(AantalMin, AantalMax)
      if (AantalMin != AantalMax) {
        warning("Voor sommige soorten of kenmerken is enkel aan- of afwezigheid opgegeven, geen bedekking, waardoor het aantal soorten dat aan een welbepaalde voorwaarde voldoet (bv. minimum een welbepaalde bedekking heeft), niet met zekerheid bepaald kan worden.  In deze gevallen is het resultaat als een range weergegeven.") #nolint
      }
    } else {
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
    }

    return(Aantal)
  }
)

setMethod(           #bij deze methode nog rekening houden met eventuele dubbels in de soortenlijst!!!
  f = "geefTheoretischMaximum",
  signature = "aantal",
  definition = function(object) {
    if (nrow(object@Soortengroep) > 0) {
      return(nrow(object@Soortengroep))
    }
    if (nrow(object@Studiegroep) > 0) {
      return(nrow(object@Studiegroep))
    }
    return(NA)
  }
)
