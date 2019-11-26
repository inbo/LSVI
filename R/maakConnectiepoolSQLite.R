#' @title Maak een connectiepool de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie maakt een connectiepool met de in het package
#' toegevoegd databank met LSVI-indicatoren, wat nodig is om de functies te
#' kunnen gebruiken.  Deze connectiepool moet eenmalig aangemaakt worden, en
#' functies zullen automatisch deze connectiepool gebruiken om te connecteren
#' met de databank.  Alternatief is om een connectie aan te maken met de
#' functie connecteerMetLSVIlite() en deze bij elke functie mee te geven.
#' Voorlopig bevat deze databank bij wijze van test maar enkele gegevens en
#' wordt best connectie gemaakt met de volledige databank via de functie
#' maakConnectiePool().
#'
#' @return Deze functie maakt een Environment-object aan dat de connecties
#' regelt met de betreffende databank in de installatie-file van het package.
#'
#' @examples
#' library(LSVI)
#' maakConnectiepoolSQLite()
#' geefVersieInfo()
#' library(pool)
#' poolClose(ConnectiePool)
#'
#' @export
#'
#' @importFrom pool dbPool
#' @importFrom RSQLite SQLite
#'

maakConnectiepoolSQLite <- function() {
  tryCatch(
    assign(
      "ConnectiePool",
      dbPool(
        drv = SQLite(),
        dbname =
          system.file("databank/LSVIHabitatTypes.sqlite", package = "LSVI"),
        encoding = "UTF-8"
      ),
      envir = .GlobalEnv
    ),
    error = function(e) {
      warning("Het lukt niet om een connectie te leggen naar de databank in het package.  Neem contact op met de beheerder van het package als dit probleem zich blijft voordoen.")  #nolint
    }
  )
}
