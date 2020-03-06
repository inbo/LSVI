#' @title Connecteer met de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie maakt een connectie met de in het package
#' toegevoegde databank met LSVI-indicatoren, wat nodig is om de functies te
#' kunnen gebruiken.  Deze connectie moet als argument meegegeven worden bij
#' elke functie functie die informatie uit de databank ophaalt.  Alternatief is
#' om eenmalig een connectiepool aan te maken met de functie
#' maakConnectiePool().
#'
#' @return Deze functie geeft een open odbc-connectie naar de SQLite-databank
#' in de installatie-file van het package.
#'
#' @examples
#' library(LSVI)
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' geefVersieInfo(ConnectieLSVIhabitats)
#' library(DBI)
#' dbGetQuery(ConnectieLSVIhabitats,
#'            "SELECT VersieLSVI, Referentie FROM Versie")
#' dbDisconnect(ConnectieLSVIhabitats)
#'
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#'

connecteerMetLSVIdb <- function() {

  ConnectieLSVIhabitats <-
    dbConnect(
      drv = SQLite(),
      dbname =
        system.file("databank/LSVIHabitatTypes.sqlite", package = "LSVI"),
      encoding = "UTF-8"
    )

  return(ConnectieLSVIhabitats)
}
