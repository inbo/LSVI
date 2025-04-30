#' @title Maak een connectiepool met de databank met LSVI-indicatoren in het
#' package
#'
#' @description Deze functie maakt een connectiepool met de in het package
#' toegevoegde databank met LSVI-indicatoren, wat nodig is om de functies te
#' kunnen gebruiken.  Deze connectiepool moet eenmalig aangemaakt worden, en
#' functies zullen automatisch deze connectiepool gebruiken om te connecteren
#' met de databank.  Alternatief is om een connectie aan te maken met de
#' functie connecteerMetLSVIdb() en deze bij elke functie mee te geven.
#'
#' @return Deze functie maakt een Environment-object aan dat de connecties
#' regelt met de betreffende databank.
#'
#' @examples
#' library(LSVI)
#' maakConnectiePool()
#' geefVersieInfo()
#' library(pool)
#' poolClose(ConnectiePool)
#'
#' @export
#'
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#'

maakConnectiePool <-
  function() {

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
      warning("Het lukt niet om een connectiepool te leggen naar de databank in het package.  Controleer of deze databank lokaal aanwezig is (bestand LSVIHabitatTypes.sqlite in folder Library/LSVI/databank) en test eventueel of een connectie leggen wel lukt met de functie connecteerMetLSVIdb().")  #nolint
    }
  )
}
