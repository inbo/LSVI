#' @title Connecteer met de databank met LSVI-indicatoren
#'
#' @description Deze functie maakt een connectie met de databank met LSVI-indicatoren.  Deze connectie moet altijd geopend worden en meegegeven als argument om andere functies te kunnen gebruiken, want alle functies binnen dit package hebben een databankconnectie nodig om gegevens uit de databank te halen.  Zonder deze connectie werken ze niet.  Binnen INBO kunnen de defaultwaarden gebruikt worden, d.w.z. dat er geen argumenten meegegeven moeten worden in connecteerMetLSVIdb().  Gebruikers buiten INBO hebben een kopie van de databank nodig om met dit package te kunnen werken, en eventueel een aangepaste versie van deze functie.
#'
#' @param Server de server waarop de databank staat die aangeroepen wordt (standaard "inbosql03\\prd")
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
  function(Server = "inbosql03\\prd",
           Databank = "D0122_03_LSVIHabitatTypes",
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
        tryCatch(Wachtwoord <- scan(file = system.file("credentials", package = "LSVI"),
                                    what = "character"),
                 error = function(e) {
                   print("Error: Geen wachtwoord gevonden")
                   return(NULL)
                 },
                 warning = function(w) {
                   print("Error: Geen wachtwoord gevonden")
                   return(NULL)
                 },
                 message = FALSE)
    }
    Connectiestring <-
      sprintf("Driver=SQL Server;Server=%s;Database=%s;UID=%s;PWD=%s",
              Server, Databank, Gebruiker, Wachtwoord)
  }



  ConnectieLSVIhabitats <- odbcDriverConnect(Connectiestring)
}
