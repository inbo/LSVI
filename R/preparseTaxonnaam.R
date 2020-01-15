#' @title parseTaxonnaam deel 1
#'
#' @description Deze functie bevat het eerste deel van de functie
#' parseTaxonnaam, namelijk de omzettingen van de naam die moeten gebeuren
#' voor het parsen door de gbif-service.
#'
#' @param Taxonnaam Wetenschappelijke naam of namen die vereenvoudigd moeten
#' worden (String)
#'
#' @return Deze functie geeft de licht aangepaste naam of namen terug (als
#' string of vector van strings)
#'

preparseTaxonnaam <- function(Taxonnaam) {

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
  Taxonnaam <-
    gsub(
      "^([A-Z][a-z]+)\\s([A-Z][a-z]*\\.?)\\s(subg.(\\s[A-Z][a-z]+\\.?)(\\s?[A-Z]?[a-z]*\\.?)*)$", #nolint
      "\\1 \\3", Taxonnaam
    )

  return(Taxonnaam)
}
