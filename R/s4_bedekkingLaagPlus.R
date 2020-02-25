#' S4-klasse die de totale bedekking van een vegetatielaag en taxongroep
#' berekent
#'
#' Deze klasse bedekkingLaagPlus staat in voor de berekening van waarden voor
#' AnalyseVariabele bedekkingLaagPlus op basis van opgegeven kenmerken.  Ze is
#' een nakomeling van de klasse bedekkingLaag.  Verschillend van bedekkingLaag
#' wordt ze gebruikt als de totale bedekking van een vegetatielaag en een
#' taxongroep berekend moet worden, bv. de totale bedekking van de moslaag en
#' klimop.  Bij het samenvoegen van de bedekkingen maakt ze gebruik van de
#' formule van Ficher (die rekening houdt met een gedeeltelijke overlap).
#' De berekening van de bedekking van de vegetatielaag maakt ze, zoals de
#' klasse bedekkingLaag, op basis van de studiegroep als deze aanwezig is in de
#' opname, en anders op basis van de soortengroep.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod as
#' @importFrom dplyr %>% arrange count filter group_by
#'
#' @noRd
#'
#' @include s4_bedekkingLaag.R
setClass(
  Class = "bedekkingLaagPlus",
  representation =
    representation(),
  contains = "bedekkingLaag"
)

setMethod(
  f = "berekenWaarde",
  signature = "bedekkingLaagPlus",
  definition = function(object) {

    Taxongroepen <- object@Soortengroep %>%
      group_by(.data$TaxonsubgroepId) %>%
      count() %>%
      arrange(.data$n)
    if (nrow(Taxongroepen) == 2) {
      objectKlein <- object
      objectKlein@Soortengroep <- objectKlein@Soortengroep %>%
        filter(.data$TaxonsubgroepId == Taxongroepen$TaxonsubgroepId[1])
      objectKlein@Studiegroep <- data.frame()
      WaardeKlein <- berekenWaarde(as(objectKlein, "bedekking"))

      objectLaag <- object
      objectLaag@Soortengroep <- objectLaag@Soortengroep %>%
        filter(.data$TaxonsubgroepId == Taxongroepen$TaxonsubgroepId[2])
      WaardeLaag <- berekenWaarde(as(objectLaag, "bedekkingLaag"))
      BedekkingMin <-
        (1.0 - prod((1.0 - c(WaardeKlein[1], WaardeLaag[1])), na.rm = TRUE))
      BedekkingMax <-
        (1.0 - prod((1.0 - c(WaardeKlein[2], WaardeLaag[2])), na.rm = TRUE))
      return(c(BedekkingMin, BedekkingMax))
    } else {
      berekenWaarde(as(object, "bedekkingLaag"))
    }
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "bedekkingLaagPlus",
  definition = function(object) {
    return(1)
  }
)
