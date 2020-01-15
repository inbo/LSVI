#' @title Vereenvoudig de soortnaam door de auteurs te verwijderen
#'
#' @description Deze functie vereenvoudigt de opgegeven taxonna(a)m(en) door
#' de auteursnaam te verwijderen.  Ze is gebaseerd op de functie parsenames
#' uit het package rgbif, maar ze vangt enkele specifieke situaties uit de
#' INBO-plantenlijsten wel op die de originele functie niet correct opvangt, bv.
#' v., an en den herkennen als deel van een auteursnaam en s.l. en meerdere
#' soortnamen gescheiden door / (zonder spatie) vervangen door een achtervoegsel
#' groep.
#'
#' @param Taxonnaam Wetenschappelijke naam of namen die vereenvoudigd moeten
#' worden (String)
#'
#' @param ParseType Welk type parsing teruggegeven moet worden.  Standaard is
#' dit 'canonicalnamewithmarker', andere opties zijn 'canonicalname' en
#' 'canonicalnamecomplete'
#'
#' @return Deze functie geeft de vereenvoudigde naam of namen terug (als string
#' of vector van strings)
#'
#' @examples
#' parseTaxonnaam("Rosa canina L.")
#'
#' @export
#'
#' @importFrom rgbif parsenames
#'

parseTaxonnaam <- function(Taxonnaam, ParseType = "canonicalnamewithmarker") {

  if (length(Taxonnaam) == 0) {
    return(as.character("geen soort opgegeven (lege vector)"))
  }
  if (all(is.na(Taxonnaam))) {
    return(rep(NA, length(Taxonnaam)))
  }
  Taxonnaam <- preparseTaxonnaam(Taxonnaam)

  ResultaatParser <- parsenames(Taxonnaam)
  Resultaat <- postparseTaxonnaam(ResultaatParser, ParseType)

  return(Resultaat)
}
