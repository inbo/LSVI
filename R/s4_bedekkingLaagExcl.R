#' S4-klasse die de totale bedekking van een vegetatielaag berekent
#'
#' Deze klasse `bedekkingLaagExcl` staat in voor de berekening van waarden voor
#' `AnalyseVariabele` `bedekkingLaagExcl` op basis van opgegeven kenmerken.
#' Ze is een nakomeling van de klasse `bedekkingLaag`.
#' Ze maakt de berekening op basis
#' van de studiegroep als deze aanwezig is in de opname, en anders op basis van
#' de soortengroep, zoals bij `bedekkingLaag`.
#' Extra is dat ze bij opgave van 2 taxongroepen de soorten uit de taxongroep
#' met het minste soorten schrapt uit de opgegeven kenmerken,
#' vooraleer de berekening uitgevoerd wordt op basis van de taxongroep met de
#' meeste soorten.
#' (Voordeel hiervan is dat de taxa op genusniveau gedefinieerd kunnen worden,
#' zodat een door de gebruiker ingevoerde genusnaam niet "genegeerd" wordt,
#' terwijl specifieke soorten wel geschrapt kunnen worden.  We nemen bv. om de
#' bedekking van de boom- en struiklaag te berekenen, het genus _Salix_ mee,
#' m.u.v. _Salix repens_.  Als de gebruiker een moeilijke _Salix_-soort tot op
#' genusniveau gedetermineerd heeft, wordt deze als boom meegeteld om de
#' totale bedekking van de boom- en struiklaag te berekenen.  Geeft de
#' gebruiker _Salix repens_ mee, dan wordt deze niet meegeteld.  Alle andere
#' _Salix_-soorten worden wel meegeteld.)
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' `Vegetatielaag`, `Kenmerk`, `TypeKenmerk`, `WaardeMin` en `WaardeMax`
#'
#' @importFrom methods setClass setMethod as
#' @importFrom dplyr %>% arrange count filter group_by
#'
#' @noRd
#'
#' @include s4_bedekkingLaag.R
setClass(
  Class = "bedekkingLaagExcl",
  representation =
    representation(),
  contains = "bedekkingLaag"
)

setMethod(
  f = "berekenWaarde",
  signature = "bedekkingLaagExcl",
  definition = function(object) {

    if (length(object@Kenmerken > 0)) {
      Taxongroepen <- object@Soortengroep %>%
        group_by(.data$TaxonGroepCode) %>%
        count() %>%
        arrange(.data$n)
      if (nrow(Taxongroepen) == 2) {
        Schrappen <- object@Soortengroep %>%
          filter(.data$TaxonGroepCode == Taxongroepen$TaxonGroepCode[1])
        for (Niveau in unique(Schrappen$Rank)) {
          Kolomnaam <- paste0(toTitleCase(tolower(Niveau)), "Key")
          object@Kenmerken <- object@Kenmerken %>%
            anti_join(
              Schrappen %>%
                filter(.data$Rank == Niveau),
              by = setNames("GbifUsageKey", Kolomnaam)
            )
        }
      }
      berekenWaarde(as(object, "bedekkingLaag"))
    } else {
      berekenWaarde(as(object, "bedekkingLaag"))
    }
  }
)
