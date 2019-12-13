#' S4-klasse die het aantal groepen telt
#'
#' Deze klasse aantalGroepen staat in voor de berekening van waarden voor
#' AnalyseVariabele aantalGroepen op basis van opgegeven kenmerken.  Ze is een
#' nakomeling van de klasse aantal.  Ze telt het aantal studieitems dat
#' opgegeven is, en als deze niet opgegeven zijn, bepaalt ze eerst waarden voor
#' de studieitems op basis van de soortenlijst.  Het gaat hier over
#' studiegroepen als levensvormen of groeivormen, en voorbeelden van studieitems
#' zijn helofyten of dwergstruiken.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod as
#'
#' @include s4_aantal.R
setClass(
  Class = "aantalGroepen",
  representation =
    representation(),
  contains = "aantal"
)

setMethod(
  f = "berekenWaarde",
  signature = "aantalGroepen",
  definition = function(object) {

    Test <- object@Kenmerken %>%
      filter(
        .data$TypeKenmerk == "studiegroep",
        tolower(.data$Kenmerk) %in% tolower(object@Studiegroep$Waarde)
      )
    
    if (nrow(Test) > 0) {
      object@Soortengroep <- data.frame()
      berekenWaarde(as(object, "aantal"))
    } else {
      object@Soortengroep$TaxonId <- object@Soortengroep$TaxonsubgroepId
      object@Soortengroep$TaxonsubgroepId <- object@Soortengroep$TaxongroepId
      object@Soortengroep$TaxongroepId <- 1
      berekenWaarde(as(object, "bedekking"))
    }
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "aantalGroepen",
  definition = function(object) {
    return(nrow(unique(object@Studiegroep)))
  }
)
