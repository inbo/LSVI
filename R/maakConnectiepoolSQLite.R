#' @title Maak een connectiepool met de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie maakt een connectiepool met de in het package toegevoegde databank met LSVI-indicatoren, net zoals de functie maakConnectiePool().  Aangeraden wordt om de functie maakConnectiePool() te gebruiken in plaats van deze functie, omdat deze functie op termijn zal verdwijnen.
#'
#' @return Deze functie maakt een Environment-object aan dat de connecties regelt met de betreffende databank in de installatie-file van het package.
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

maakConnectiepoolSQLite <- function(){
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
