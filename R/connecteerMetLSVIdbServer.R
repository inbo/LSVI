#' @title Connecteer met de databank met LSVI-indicatoren op databankserver
#'
#' @description Deze interne functie maakt een connectie met de databank met
#' LSVI-indicatoren op de databankserver binnen INBO (werking gelijkaardig aan
#' connecteerMetLSVIdb).  Deze functie is enkel bedoeld voor ontwikkeling van
#' het package; niet alle functies werken correct bij gebruik van deze databank.
#'
#' @param Server de server waarop de databank staat die aangeroepen wordt
#' (standaard "INBO-SQL07-PRD.inbo.be")
#' @param Databank de naam van de databank die aangeroepen wordt (standaard
#' "D0122_00_LSVIHabitatTypes")
#'
#' @return Deze functie geeft een open odbc-connectie naar de databank
#' D0122_00_LSVIHabitatTypes op de Amazon-server van INBO.
#'
#' @noRd
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#'

connecteerMetLSVIdbServer <-
  function(Server = "INBO-SQL07-PRD.inbo.be",
           Databank = "D0122_00_LSVIHabitatTypes") {

  assert_that(is.string(Server))
  assert_that(is.string(Databank))

  ConnectieLSVIhabitats <-
    dbConnect(
      odbc(),
      Driver = "SQL Server",
      Server = Server,
      Database = Databank,
      Trusted_Connection = "True",
      encoding = "UTF-8"
    )

  return(ConnectieLSVIhabitats)
}
