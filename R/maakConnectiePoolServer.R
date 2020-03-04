#' @title Maak een connectiepool met de databank met LSVI-indicatoren op de
#' databankserver
#'
#' @description Deze interne functie maakt een connectiepool met de databank 
#' met LSVI-indicatoren op de databankserver binnen INBO (werking gelijkaardig
#' aan maakConnectiePool).  Deze functie is enkel bedoeld voor ontwikkeling van
#' het package; niet alle functies werken correct bij gebruik van deze databank.
#'
#' @inheritParams connecteerMetLSVIdbServer
#' @return Deze functie maakt een Environment-object aan dat de connecties
#' regelt met de betreffende databank.
#'
#' @noRd
#'
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#'

maakConnectiePoolServer <-
  function(Server = "INBO-SQL07-PRD.inbo.be",
           Databank = "D0122_00_LSVIHabitatTypes") {
  assert_that(is.string(Server))
  assert_that(is.string(Databank))

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
  )
}
