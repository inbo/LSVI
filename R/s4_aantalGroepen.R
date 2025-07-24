#' S4-klasse die het aantal groepen telt
#'
#' Deze klasse `aantalGroepen` staat in voor de berekening van waarden voor
#' `AnalyseVariabele` `aantalGroepen` op basis van opgegeven kenmerken.
#' Ze is een nakomeling van de klasse `aantal`.
#' Ze telt het aantal kenmerken dat
#' opgegeven is, en als deze niet opgegeven zijn, bepaalt ze eerst waarden voor
#' de kenmerken op basis van de soortenlijsten die aan de kenmerken hangen.
#' Het gaat hier over
#' studiegroepen als levensvormen of groeivormen, en voorbeelden van kenmerken
#' zijn helofyten of dwergstruiken.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' `Vegetatielaag`, `Kenmerk`, `TypeKenmerk`, `WaardeMin` en `WaardeMax`
#'
#' @importFrom dplyr %>% bind_rows filter
#' @importFrom methods setClass setMethod as
#'
#' @noRd
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
      object@Studiegroep <- data.frame()
      Resultaat <-
        data.frame(TaxonGroepCode = character(0), Aantal = integer(0))
      for (Groep in unique(object@Soortengroep$TaxonGroepCode)) {
        Deelobject <- object
        Deelobject@Soortengroep <- Deelobject@Soortengroep %>%
          filter(.data$TaxonGroepCode == Groep)
        Res <- berekenWaarde(as(Deelobject, "aantal"))
        Resultaat <- Resultaat %>%
          bind_rows(data.frame(TaxonGroepCode = Groep, Aantal = Res))
      }
      Resultaat <- Resultaat %>%
        filter(!is.na(.data$Aantal), .data$Aantal > 0)
      Aantal <- nrow(Resultaat)

      return(Aantal)
    }
  }
)
