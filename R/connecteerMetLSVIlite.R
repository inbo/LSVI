#' @title Connecteer met de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie maakt een connectie met de in het package geïntegreerde databank met LSVI-indicatorendatabank, wat nodig is om de functies te kunnen gebruiken.  Voorlopig bevat deze geïntegreerde databank bij wijze van test maar enkele gegevens en wordt best connectie gemaakt met de volledige databank via de functie connecteerMet LSVIdb().  (Deze functie is zodanig ingebouwd in de code dat ze niet expliciet opgegeven moet worden door de gebruiker, tenzij deze een andere databank wil opgeven.)
#'
#' @return Deze functie geeft een open SQLite-connectie naar de betreffende databank in de installatie-file van het package.
#'
#' @examples
#' library(LSVI)
#' library(DBI)
#' ConnectieLSVIhabitats <- connecteerMetLSVIlite()
#' dbGetQuery(ConnectieLSVIhabitats, "SELECT VersieLSVI, Referentie FROM Versie")
#' dbDisconnect(ConnectieLSVIhabitats)
#'
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#'

connecteerMetLSVIlite <- function(){

  ConnectieLSVIhabitats <-
    dbConnect(
      drv = SQLite(),
      dbname =
        system.file("databank/LSVIHabitatTypes.sqlite", package = "LSVI"),
      encoding = "UTF-8"
    )

  return(ConnectieLSVIhabitats)
}
