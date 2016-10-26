#' @title Connecteer met de databank met LSVI-indicatoren
#'
#' @description Deze functie maakt een connectie met de databank met LSVI-indicatoren.  Bedoeling van deze aparte functie is om de link naar deze databank maar op 1 plaats te bewaren, zodat deze bij het verplaatsen van de databank ook maar op een plaats aangepast moet worden.  Deze link wordt als argument meegegeven om gebruikers toe te laten om deze aan te passen.  (Deze link zou eigenlijk ook aanpasbaar moeten zijn in de functies die deze aanroepen!)
#'
#' @param Server de server waarop de databank staat (standaard "inbosql03\\prd")
#' @param Databank de naam van de databank (standaard "D0122_00_LSVIHabitatTypes")
#' @param Gebruiker standaard een gebruiker met leesrechten, andere opties zijn "pc-eigenaar" waarbij de login en wachtwoord gebruikt wordt waarmee ingelogd is op de pc of een eigen gebruikersnaam en wachtwoord ingeven
#' @param Wachtwoord standaard een gebruiker met leesrechten, moet niet ingevuld worden als Gebruiker "pc-eigenaar" is
#'
#' @return Deze functie geeft een open odbc-connectie naar de gespecifieerde databank.
#' 
#' @examples 
#' library(RODBC)
#' connectie <- connecteerMetLSVIdb()
#' sqlQuery(connectie, "SELECT VersieLSVI, Referentie FROM Versie")
#' odbcClose(connectie)
#'
#' @export
#'
#' @importFrom RODBC odbcDriverConnect
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#'

connecteerMetLSVIdb <- 
  function(Server = "inbosql03\\prd",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "D0122_AppR",
           Wachtwoord = "***REMOVED***"){
    
  # assert_that(is.string(dns))
  # if (!file_test("-f", dns)) {
  #   stop("File does not exist")
  # }
  
  if(Gebruiker == "pc-eigenaar"){
    Connectiestring <- 
      sprintf("Driver=SQL Server;Server=%s;Database=%s;Trusted_Connection=Yes;",
              Server, Databank)
  } else {
    Connectiestring <- 
      sprintf("Driver=SQL Server;Server=%s;Database=%s;UID=%s;PWD=%s",
              Server, Databank, Gebruiker, Wachtwoord)
  } 
      
    
  
  connectie <- odbcDriverConnect(Connectiestring)
}
