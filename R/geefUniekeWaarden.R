#' @title Geef alle unieke waarden van een veld uit de databank met de LSVI-indicatoren voorafgegaan door "alle"
#'
#' @description Deze functie geeft een vector met alle verschillende waarden die een gespecifieerd veld van een gespecifieerde tabel in de databank met LSVI-indicatoren staan, voorafgegaan door de (toegevoegde) waarde "alle".  Deze functie wordt in verschillende functies van het package gebruikt om de invoer van parameters te controleren (waar de mogelijke invoer bestaat uit 'alle' of een item uit het veld).
#'
#' @inheritParams selecteerIndicatoren
#' 
#' @param Tabelnaam De naam van de tabel waarin het veld zich bevindt (String)
#'
#' @param Veldnaam De naam van het veld (in de bij Tabelnaam opgegeven tabel) waarvan de waarden moeten opgezocht worden (String)
#'
#' @return Deze functie geeft een vector bestaande uit "alle" en de verschillende waarden uit de gespecifieerde tabel.
#' 
#' @examples 
#' geefUniekeWaarden("Habitatgroep","Naam")
#'
#' @export
#'
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that is.string noNA
#'

geefUniekeWaarden <-
  function(Tabelnaam,
           Veldnaam,
           ConnectieLSVIhabitats = ConnectiePool) {

  assert_that(
    inherits(ConnectieLSVIhabitats, "DBIConnection") |
      inherits(ConnectieLSVIhabitats, "Pool"),
    msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
  )
  assert_that(is.string(Tabelnaam))
  assert_that(noNA(Tabelnaam))
  assert_that(is.string(Veldnaam))
  assert_that(noNA(Veldnaam))

  query <- sprintf("SELECT %s FROM %s", Veldnaam, Tabelnaam)
  Waarden <- dbGetQuery(ConnectieLSVIhabitats, query)
  UniekeWaarden <- c("alle", unique(Waarden[, Veldnaam]))

  return(UniekeWaarden)
}
