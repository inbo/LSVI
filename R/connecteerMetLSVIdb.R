#' @title Connecteer met de databank met LSVI-indicatoren
#'
#' @description Deze functie maakt een connectie met de databank met
#' LSVI-indicatoren, wat nodig is om de functies te kunnen gebruiken.
#' Voorlopig verwijst deze naar een databank binnen INBO, waardoor gebruikers
#' buiten INBO een kopie van de databank nodig hebben om met dit package te
#' kunnen werken, en eventueel een aangepaste versie van deze functie.  Op
#' termijn zal deze databank toegevoegd worden aan het package, waardoor ze
#' overal zou moeten werken.  (Deze functie is zodanig ingebouwd in de code dat
#' ze niet expliciet opgegeven moet worden door de gebruiker, tenzij deze een
#' andere databank wil opgeven.)
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
#' @return Deze functie geeft een open odbc-connectie naar de gespecifieerde
#' databank.
#'
#' @examples
#' # deze functie, en dus ook onderstaande code, kan enkel gerund worden als er
#' # een connectie gelegd kan worden met de SQL Server-databank binnen INBO
#' \dontrun{
#' library(DBI)
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' dbGetQuery(ConnectieLSVIhabitats,
#'            "SELECT VersieLSVI, Referentie FROM Versie")
#' dbDisconnect(ConnectieLSVIhabitats)
#' }
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
    ConnectieLSVIhabitats <-
      dbConnect(
        odbc(),
        Driver = "SQL Server",
        Server = Server,
        Database = Databank,
        Trusted_Connection = "True",
        encoding = "UTF-8"
      )
  } else {
    if (Gebruiker == "lezer") {
      Gebruiker <- "D0122_AppR"
      Wachtwoord <-
        tryCatch(
          Wachtwoord <-
            scan(
              file = system.file("credentials", package = "LSVI"),
              what = "character"
            ),
          error = function(e) {
            print("Error: Geen wachtwoord gevonden")
            return(NULL)
          },
          warning = function(w) {
            print("Error: Geen wachtwoord gevonden")
            return(NULL)
          },
          message = FALSE
        )
    }
    ConnectieLSVIhabitats <-
      dbConnect(
        odbc(),
        Driver = "SQL Server",
        Server = Server,
        Database = Databank,
        UID = Gebruiker,
        PWD = Wachtwoord,
        encoding = "UTF-8"
      )
  }

  return(ConnectieLSVIhabitats)
}
