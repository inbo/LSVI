#' @title Connecteer met de databank met LSVI-indicatoren
#'
#' @description Deze functie maakt een connectie met de databank met LSVI-indicatorendatabank, wat nodig is om de functies te kunnen gebruiken.  Voorlopig verwijst deze naar een databank binnen INBO, waardoor gebruikers buiten INBO een kopie van de databank nodig hebben om met dit package te kunnen werken, en eventueel een aangepaste versie van deze functie.  Op termijn zal deze databank geïntegreerd worden in het package, waardoor ze overal zou moeten werken.  (Deze functie is zodanig ingebouwd in de code dat ze niet expliciet opgegeven moet worden door de gebruiker, tenzij deze een andere databank wil opgeven.)
#'
#' @param Server de server waarop de databank staat die aangeroepen wordt (standaard "INBO-SQL07-PRD.inbo.be")
#' @param Databank de naam van de databank die aangeroepen wordt (standaard "D0122_00_LSVIHabitatTypes")
#' @param Gebruiker gebruiker van de databank, standaard "pc-eigenaar" waarbij de login en wachtwoord gebruikt wordt waarmee ingelogd is op de pc, andere opties zijn "lezer" waarbij een gebruiker met leesrechten aangemaakt wordt (dit kan enkel als dit geïnstalleerd is op je pc) of een eigen gebruikersnaam en wachtwoord ingeven
#' @param Wachtwoord wachtwoord van de gebruiker van de databank, standaard de tekst "geen", moet niet ingevuld worden als Gebruiker "pc-eigenaar" is
#'
#' @return Deze functie geeft een open odbc-connectie naar de gespecifieerde databank.
#'
#' @examples
#' library(RODBC)
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' sqlQuery(ConnectieLSVIhabitats, "SELECT VersieLSVI, Referentie FROM Versie")
#' odbcClose(ConnectieLSVIhabitats)
#'
#' @export
#'
#' @importFrom RODBC odbcDriverConnect
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#'

connecteerMetLSVIdb <-
  function(Server = "INBO-SQL07-PRD.inbo.be",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "pc-eigenaar",
           Wachtwoord = "geen"){

  assert_that(is.string(Server))
  assert_that(is.string(Databank))
  assert_that(is.string(Gebruiker))
  assert_that(is.string(Wachtwoord))

  if (Gebruiker == "pc-eigenaar") {
    Connectiestring <-
      sprintf("Driver=SQL Server;Server=%s;Database=%s;Trusted_Connection=Yes;",
              Server, Databank)
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
    Connectiestring <-
      sprintf("Driver=SQL Server;Server=%s;Database=%s;UID=%s;PWD=%s",
              Server, Databank, Gebruiker, Wachtwoord)
  }

  ConnectieLSVIhabitats <- odbcDriverConnect(Connectiestring)
  return(ConnectieLSVIhabitats)
}
