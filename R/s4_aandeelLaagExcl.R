#' S4-klasse die het aandeel t.o.v. de totale vegetatielaag berekent
#'
#' Deze klasse `aandeelLaagExcl` staat in voor de berekening van waarden voor
#' `AnalyseVariabele` `aandeelLaagExcl` op basis van opgegeven kenmerken.
#' Ze is een nakomeling van de klasse `aandeelKruidlaag`.
#' Ze berekent de bedekking van de opgegeven soorten ten opzichte van de in de
#' studiegroep opgegeven vegetatielaag, zoals `aandeelKruidlaag.`
#' Extra is dat ze bij opgave van 2
#' taxongroepen de soorten uit de taxongroep met het minste soorten schrapt uit
#' de taxongroep met het meeste soorten om een nieuwe soortengroep te bekomen.
#' (Voordeel hiervan is dat de taxa op genusniveau gedefinieerd kunnen worden,
#' zodat een door de gebruiker ingevoerde genusnaam niet "genegeerd" wordt,
#' terwijl specifieke soorten wel geschrapt kunnen worden.
#' Deze berekening wordt bijvoorbeeld gebruikt om het aandeel vergrassing
#' exclusief een bepaalde soort te berekenen t.o.v. de totale vegetatiebedekking
#' in de duinen.)
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' `Vegetatielaag`, `Kenmerk`, `TypeKenmerk`, `WaardeMin` en `WaardeMax`
#'
#' @importFrom methods setClass setMethod as
#' @importFrom dplyr %>% arrange count filter group_by
#'
#' @noRd
#'
#' @include s4_aandeelKruidlaag.R
setClass(
  Class = "aandeelLaagExcl",
  representation =
    representation(),
  contains = "aandeelKruidlaag"
)

setMethod(
  f = "berekenWaarde",
  signature = "aandeelLaagExcl",
  definition = function(object) {

    Taxongroepen <- object@Soortengroep %>%
      group_by(.data$TaxonsubgroepId) %>%
      count() %>%
      arrange(.data$n)
    if (nrow(Taxongroepen) == 2) {
      Schrappen <- object@Soortengroep %>%
        filter(.data$TaxonsubgroepId == Taxongroepen$TaxonsubgroepId[1])
      object@Soortengroep <- object@Soortengroep %>%
        filter(!.data$NbnTaxonVersionKey %in% Schrappen$NbnTaxonVersionKey)
      berekenWaarde(as(object, "aandeelKruidlaag"))
    } else {
      berekenWaarde(as(object, "aandeelKruidlaag"))
    }
  }
)
