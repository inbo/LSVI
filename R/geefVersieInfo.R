#' @title Geeft informatie over de verschillende versies voor de berekening van de LSVI
#'
#' @description Deze functie geeft een overzicht van alle versies die er zijn voor de berekening van de de Lokale Staat van Instandhouding, met naast de opsomming van de versies en de referenties een overzicht van de 2 kwaliteitsniveaus of types van beoordelingscriteria die in deze versie gedefinieerd zijn.
#' 
#' @return Deze functie geeft de tabel Versie uit de databank.
#' 
#' @examples 
#' geefVersieInfo()
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#'
#'
geefVersieInfo <- function(){
  query <- "SELECT VersieLSVI, Referentie, Beschrijving, 
  Kwaliteitsniveau1, Kwaliteitsniveau2 FROM Versie"
  
  connectie <- connecteerMetLSVIdb()
  Versie <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
  odbcClose(connectie)
  
  return(Versie)
}