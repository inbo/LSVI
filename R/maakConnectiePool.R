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
#' @inheritParams connecteerMetLSVIdb
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
  function(Server = "INBO-SQL07-PRD.inbo.be",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "pc-eigenaar",
           Wachtwoord = "geen") {
  assert_that(is.string(Server))
  assert_that(is.string(Databank))
  assert_that(is.string(Gebruiker))
  assert_that(is.string(Wachtwoord))
  assert_that(
    packageVersion("odbc") <= package_version("1.2.0"),
    msg = "Het LSVI-package geeft problemen met de nieuwste versie van odbc. Installeer een oudere versie met het commando install.packages('http://cran.r-project.org/src/contrib/Archive/odbc/odbc_1.1.6.tar.gz', repos = NULL, type = 'source')" #nolint
  )

  if (Gebruiker == "pc-eigenaar") {
    tryCatch(
      assign(
        "ConnectiePool",
        dbPool(
          drv = odbc(),
          Driver = "SQL Server",
          Database = Databank,
          Server = Server,
          Trusted_Connection = "TRUE"
        ),
        envir = .GlobalEnv
      ),
      error = function(e) {
        maakConnectiepoolSQLite()
      }
    )
  } else {
    maakConnectiepoolSQLite()
    message("Connectie gelegd naar de databank in het package")
  }
}
