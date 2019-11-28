#' S4-klasse die de totale bedekking van een vegetatielaag berekent
#'
#' Deze klasse bedekkingLaagExcl staat in voor de berekening van waarden voor
#' AnalyseVariabele bedekkingLaagExcl op basis van opgegeven kenmerken.  Ze is
#' een nakomeling van de klasse bedekkingLaag.  Ze maakt de berekening op basis
#' van de studiegroep als deze aanwezig is in de opname, en anders op basis van
#' de soortengroep, zoals bedekkingLaag. Extra is dat ze bij opgave van 2
#' taxongroepen de soorten uit de taxongroep met het minste soorten schrapt uit
#' de taxongroep met het meeste soorten om een nieuwe soortengroep te bekomen.
#' (Voordeel hiervan is dat de taxa op genusniveau gedefinieerd kunnen worden,
#' zodat een door de gebruiker ingevoerde genusnaam niet 'genegeerd' wordt,
#' terwijl specifieke soorten wel geschrapt kunnen worden.  We nemen bv. om de
#' bedekking van de boom- en struiklaag te berekenen, het genus Salix mee,
#' m.u.v. Salix repens.  Als de gebruiker een moeilijke Salix-soort tot op
#' genusniveau gedetermineerd heeft, wordt deze als boom meegeteld om de
#' totale bedekking van de boom- en struiklaag te berekenen.  Geeft de
#' gebruiker Salix repens mee, dan wordt deze niet meegeteld.  Alle andere
#' Salix-soorten worden wel meegeteld.)
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod as
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

    Taxongroepen <- object@Soortengroep %>%
      group_by(.data$TaxonsubgroepId) %>%
      count() %>%
      arrange(.data$n)
    if (nrow(Taxongroepen) == 2) {
      Schrappen <- object@Soortengroep %>%
        filter(.data$TaxonsubgroepId == Taxongroepen$TaxonsubgroepId[1])
      object@Soortengroep <- object@Soortengroep %>%
        filter(!.data$NbnTaxonVersionKey %in% Schrappen$NbnTaxonVersionKey)
      berekenWaarde(as(object, "bedekkingLaag"))
    } else {
      berekenWaarde(as(object, "bedekkingLaag"))
    }
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "bedekkingLaagExcl",
  definition = function(object) {
    return(1)
  }
)
