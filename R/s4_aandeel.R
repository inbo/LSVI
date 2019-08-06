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

    sleutelsoorten <-
      selecteerKenmerkenInOpname(
        object@Kenmerken,
        object@Soortengroep,
        object@Studiegroep,
        object@SubAnalyseVariabele,
        object@SubRefMin,
        object@SubRefMax,
        object@SubOperator
      ) %>%
      filter(tolower(.data$Eenheid) %in% c("grondvlak_ha", "volume_ha"))

    alle_soorten <- object@Kenmerken %>%
      filter(tolower(.data$Eenheid) %in% c("grondvlak_ha", "volume_ha"))


    if (nrow(alle_soorten) == 0) {
      return(0)
    } else{
      teller_min <- sum(sleutelsoorten$WaardeMin, na.rm = TRUE)
      teller_max <- sum(sleutelsoorten$WaardeMax, na.rm = TRUE)
      noemer_min <- sum(alle_soorten$WaardeMin, na.rm = TRUE)
      noemer_max <- sum(alle_soorten$WaardeMax, na.rm = TRUE)

      aandeel_min <- min(teller_min / noemer_max, 1)
      aandeel_max <- min(teller_max / noemer_min, 1)
      resultaat <- c(aandeel_min, aandeel_max)
      return(resultaat)

    }
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "aandeel",
  definition = function(object) {
    return(1)
  }
)
