#' @title Vereenvoudig de soortnaam door de auteurs te verw
#'
#' @description Deze functie vereenvoudigt de opgegeven taxonna(a)m(en) door
#' de auteursnaam te verwijderen.  Ze is gebaseerd op de functie parsenames
#' uit het package rgbif, maar ze vangt enkele specifieke situaties uit de
#' INBO-plantenlijsten wel op die de originele functie niet opvangt, bv.
#' toevoegen van subsp.
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

  Resultaat <- parsenames(Taxonnaam)[, c(ParseType)]

  return(Resultaat)
}
