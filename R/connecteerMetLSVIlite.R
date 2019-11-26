#' @title Connecteer met de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie maakt een connectie met de in het package
#' toegevoegde databank met LSVI-indicatoren, net zoals de functie
#' connecteerMetLSVIdb().
#' Aangeraden wordt om de functie connecteerMetLSVIdb() te gebruiken in plaats
#' van deze functie, omdat deze functie op termijn zal verdwijnen.
#'
#' @return Deze functie geeft een open odbc-connectie naar de SQLite-databank
#' in de installatie-file van het package.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar het voorbeeld werkt en kan zeker
#' # uitgetest worden.
#' \dontrun{
#' library(LSVI)
#' library(DBI)
#' ConnectieLSVIhabitats <- connecteerMetLSVIlite()
#' dbGetQuery(ConnectieLSVIhabitats,
#'            "SELECT VersieLSVI, Referentie FROM Versie")
#' dbDisconnect(ConnectieLSVIhabitats)
#' }
#'
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#'

connecteerMetLSVIlite <- function() {

  ConnectieLSVIhabitats <-
    dbConnect(
      drv = SQLite(),
      dbname =
        system.file("databank/LSVIHabitatTypes.sqlite", package = "LSVI"),
      encoding = "UTF-8"
    )

  return(ConnectieLSVIhabitats)
}
