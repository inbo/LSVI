#' @title Geeft informatie over de verschillende versies voor de berekening van de LSVI
#'
#' @description Deze functie geeft een overzicht van alle versies die er zijn voor de berekening van de de Lokale Staat van Instandhouding, met naast de opsomming van de versies en de referenties een overzicht van de 2 kwaliteitsniveaus of types van beoordelingscriteria die in deze versie gedefinieerd zijn.  Ze connecteert hiervoor met de opgegeven databank.
#' 
#' @inheritParams connecteerMetLSVIdb
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
geefVersieInfo <- function(Server = "inbosql03\\prd",
                           Databank = "D0122_00_LSVIHabitatTypes",
                           Gebruiker = "D0122_AppR",
                           Wachtwoord = "19D939F1-BCCE-439F-9ED4-6A886E038A6D"){
  query <- "SELECT VersieLSVI, Referentie, Beschrijving, 
  Kwaliteitsniveau1, Kwaliteitsniveau2 FROM Versie"
  
  connectie <- connecteerMetLSVIdb(Server, Databank, Gebruiker, Wachtwoord)
  Versie <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
  odbcClose(connectie)
  
  return(Versie)
}