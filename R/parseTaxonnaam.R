#' @title Vereenvoudig de soortnaam door de auteurs te verw
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

  if (all(is.na(Taxonnaam))) {
    return(rep(NA, length(Taxonnaam)))
  }

  Taxonnaam <- gsub("v\\.d\\.", "v. d.", Taxonnaam)
  Taxonnaam <- gsub(" v\\.", " Van", Taxonnaam)
  Taxonnaam <- gsub(" non ", " Non", Taxonnaam)
  Taxonnaam <- gsub(" auct\\. ", " Auct. ", Taxonnaam)
  Taxonnaam <- gsub(" auct\\.$", " Auct.", Taxonnaam)
  Taxonnaam <- gsub(" auct\\., ", " ", Taxonnaam)
  Taxonnaam <- gsub(" den ", " Den ", Taxonnaam)
  Taxonnaam <- gsub(" an ", " An ", Taxonnaam)
  Taxonnaam <- gsub(" anon ", " Anon ", Taxonnaam)
  Taxonnaam <- gsub(" f.$", "", Taxonnaam)
  Taxonnaam <- gsub(" f.)$", ")", Taxonnaam)
  Taxonnaam <- gsub(" nom\\. illegit\\.$", " nom. illeg.", Taxonnaam)
  Taxonnaam <- gsub(" nom\\. superfl\\.$", "", Taxonnaam)
  Taxonnaam <- gsub(" sensu lato$", " s.l.", Taxonnaam)
  Taxonnaam <-
    gsub("^([A-Z][a-z]+\\s[a-z]+)(\\/[a-z]+)+$", "\\1 s.l.", Taxonnaam)
  Resultaat <- parsenames(Taxonnaam)
  if ("sensu" %in% colnames(Resultaat)) {
    Resultaat[, c(ParseType)] <-
      trimws(
        paste(
          Resultaat[, c(ParseType)],
          ifelse(is.na(Resultaat$sensu) | Resultaat$sensu == "s.s.", "",
                 ifelse(Resultaat$sensu == "s.l.", "groep", Resultaat$sensu))
        )
      )
  }
  Resultaat <- Resultaat[, c(ParseType)]
  Resultaat <- gsub("\U00D7", "x", Resultaat)
  Resultaat <- gsub("^NA$", NA, Resultaat)

  return(Resultaat)
}
