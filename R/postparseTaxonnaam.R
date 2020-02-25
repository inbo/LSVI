#' @title parseTaxonnaam deel 3
#'
#' @description Deze functie bevat het derde deel van de functie
#' parseTaxonnaam, namelijk de omzettingen van de naam die moeten gebeuren
#' na het parsen door de gbif-service.
#'
#' @param ResultaatParser Dataframe dat een resultaat is dat teruggegeven is
#' door de gbif-parser
#'
#' @param ParseType Welk type parsing teruggegeven moet worden.  Standaard is
#' dit 'canonicalnamewithmarker', andere opties zijn 'canonicalname' en
#' 'canonicalnamecomplete'
#'
#' @return Deze functie geeft de licht aangepaste naam of namen terug (als
#' string of vector van strings)
#'
#' @noRd
#'

postparseTaxonnaam <-
  function(ResultaatParser, ParseType) {

  if ("sensu" %in% colnames(ResultaatParser)) {
    ResultaatParser[, c(ParseType)] <-
      trimws(
        paste(
          ResultaatParser[, c(ParseType)],
          ifelse(
            is.na(ResultaatParser$sensu) | ResultaatParser$sensu != "s.l.",
            "", "groep"
          )
        )
      )
  }
  Resultaat <- ResultaatParser[, c(ParseType)]
  Resultaat <- gsub("\U00D7", "x", Resultaat)
  Resultaat <- gsub("^NA$", NA, Resultaat)

  return(Resultaat)
}
