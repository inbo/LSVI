#' @title Geeft informatie over de verschillende versies voor de berekening van de LSVI
#'
#' @description Deze functie geeft een overzicht van alle versies die er zijn voor de berekening van de de Lokale Staat van Instandhouding, met naast de opsomming van de versies en de referenties een overzicht van de 2 kwaliteitsniveaus of types van beoordelingscriteria die in deze versie gedefinieerd zijn.
#'
#' @inheritParams selecteerIndicatoren
#'
#' @return Deze functie geeft de tabel Versie uit de databank.
#'
#' @examples
#' maakConnectiePool()
#' geefVersieInfo()
#'
#' @export
#'
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#'
#'
geefVersieInfo <-
  function(ConnectieLSVIhabitats = ConnectiePool){

  assert_that(
    inherits(ConnectieLSVIhabitats, "DBIConnection") |
      inherits(ConnectieLSVIhabitats, "Pool"),
    msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
  )

  query <- "SELECT VersieLSVI, cast(Referentie AS nvarchar(40)) AS Referentie,
  cast(Beschrijving AS nvarchar (120)) AS Beschrijving,
  Kwaliteitsniveau1, Kwaliteitsniveau2 FROM Versie"

  Versie <- dbGetQuery(ConnectieLSVIhabitats, query)

  return(Versie)
}
