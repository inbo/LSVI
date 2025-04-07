#' @title Vereenvoudig de soortnaam door de auteurs te verwijderen
#'
#' @description
#' __Deze functie wordt niet meer onderhouden en zal bij een volgende versie
#' van dit package verdwijnen.
#' Voor een functie met gelijkaardige functionaliteit verwijzen we naar
#' `rgbif::name_parse()` waarrond `parseTaxonnaam()` een wrapper is.__
#'
#' Deze functie vereenvoudigt de opgegeven taxonnaam of taxonnamen door
#' de auteursnaam te verwijderen.  Ze is gebaseerd op de functie `name_parse()`
#' uit het package rgbif, maar ze vangt enkele specifieke situaties op uit
#' plantenlijsten die op INBO gebruikt worden en die de originele functie niet
#' correct opvangt, bv.
#' `"v."`, `"an"` en `"den"` herkennen als deel van een auteursnaam en "s.l." en
#' meerdere soortnamen gescheiden door `"/"` (zonder spatie) vervangen door een
#' achtervoegsel "groep".
#'
#' @param Taxonnaam Wetenschappelijke naam of namen die vereenvoudigd moeten
#' worden (String)
#'
#' @param ParseType Welk type parsing teruggegeven moet worden.  Standaard is
#' dit `"canonicalnamewithmarker"`, andere opties zijn `"canonicalname"` en
#' `"canonicalnamecomplete"`
#'
#' @return Deze functie geeft de vereenvoudigde naam of namen terug (als string
#' of vector van strings)
#'
#' @examples
#' parseTaxonnaam("Rosa canina L.")
#'
#' @export
#' @rdname package-deprecated
#'
#' @importFrom rgbif name_parse
#'

parseTaxonnaam <- function(Taxonnaam, ParseType = "canonicalnamewithmarker") {

  .Deprecated("rgbif::name_parse")

  if (length(Taxonnaam) == 0) {
    return(as.character("geen soort opgegeven (lege vector)"))
  }
  if (all(is.na(Taxonnaam))) {
    return(rep(NA, length(Taxonnaam)))
  }
  Taxonnaam <- preparseTaxonnaam(Taxonnaam)

  ResultaatParser <- name_parse(Taxonnaam)
  Resultaat <- postparseTaxonnaam(ResultaatParser, ParseType)

  return(Resultaat)
}
