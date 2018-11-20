#' @title Geeft informatie over de verschillende versies voor de berekening van de LSVI
#'
#' @description Deze functie geeft een overzicht van alle versies die er zijn voor de berekening van de de Lokale Staat van Instandhouding, met naast de opsomming van de versies en de referenties een overzicht van de 2 kwaliteitsniveaus of types van beoordelingscriteria die in deze versie gedefinieerd zijn.
#'
#' @inheritParams selecteerIndicatoren
#'
#' @return Deze functie geeft de tabel Versie uit de databank.
#'
#' @examples
#' # deze functie, en dus ook onderstaande code, kan enkel gerund worden als er
#' # een connectie gelegd kan worden met de SQL Server-databank binnen INBO
#' \dontrun{
#' maakConnectiePool()
#' geefVersieInfo()
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#'
#'
geefVersieInfo <-
  function(ConnectieLSVIhabitats = NULL){

    if (is.null(ConnectieLSVIhabitats)) {
      if (exists("ConnectiePool")) {
        ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
      }
    }
    assert_that(
    inherits(ConnectieLSVIhabitats, "DBIConnection") |
      inherits(ConnectieLSVIhabitats, "Pool"),
    msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint
  )

  query <- "SELECT VersieLSVI, cast(Referentie AS nvarchar(40)) AS Referentie,
  cast(Beschrijving AS nvarchar (120)) AS Beschrijving,
  Kwaliteitsniveau1, Kwaliteitsniveau2 FROM Versie"

  Versie <- dbGetQuery(ConnectieLSVIhabitats, query)

  return(Versie)
}
