#' @title Connecteer met de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie maakt een connectie met de in het package
#' toegevoegde databank met LSVI-indicatoren, wat nodig is om de functies te
#' kunnen gebruiken.  Deze connectie moet als argument meegegeven worden bij
#' elke functie functie die informatie uit de databank ophaalt.  Alternatief is
#' om eenmalig een connectiepool aan te maken met de functie
#' maakConnectiePool().
#'
#' @param Server de server waarop de databank staat die aangeroepen wordt
#' (standaard "INBO-SQL07-PRD.inbo.be")
#' @param Databank de naam van de databank die aangeroepen wordt (standaard
#' "D0122_00_LSVIHabitatTypes")
#' @param Gebruiker gebruiker van de databank, standaard "pc-eigenaar" waarbij
#' de login en wachtwoord gebruikt wordt waarmee ingelogd is op de pc, andere
#' opties zijn "lezer" waarbij een gebruiker met leesrechten aangemaakt wordt
#' (dit kan enkel als er een login-file is toegevoegd op je pc) of een eigen
#' gebruikersnaam en wachtwoord ingeven
#' @param Wachtwoord wachtwoord van de gebruiker van de databank, standaard de
#' tekst "geen", moet niet ingevuld worden als Gebruiker "pc-eigenaar" is
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
#' @importFrom odbc odbc
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#'

connecteerMetLSVIdb <-
  function(Server = "INBO-SQL07-PRD.inbo.be",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "pc-eigenaar",
           Wachtwoord = "geen") {

  assert_that(is.string(Server))
  assert_that(is.string(Databank))
  assert_that(is.string(Gebruiker))
  assert_that(is.string(Wachtwoord))

  if (Gebruiker == "pc-eigenaar") {
    tryCatch(
      assign(
        "ConnectieLSVIhabitats",
        dbConnect(
          odbc(),
          Driver = "SQL Server",
          Server = Server,
          Database = Databank,
          Trusted_Connection = "True",
          encoding = "UTF-8"
        ),
        envir = .GlobalEnv
      ),
      error = function(e) {
        assign(
          "ConnectieLSVIhabitats",
          connecteerMetLSVIlite(),
          envir = .GlobalEnv
        )
      }
    )
  } else {
    ConnectieLSVIhabitats <- connecteerMetLSVIlite()
  }

  return(ConnectieLSVIhabitats)
}
