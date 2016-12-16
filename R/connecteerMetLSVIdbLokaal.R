#' @title Connecteer met de databank met LSVI-indicatoren
#'
#' @description Deze functie maakt een connectie met de databank met LSVI-indicatoren.  Bedoeling van deze aparte functie is om de link naar deze databank maar op 1 plaats te bewaren, zodat deze bij het verplaatsen van de databank ook maar op een plaats aangepast moet worden.  Deze link wordt als argument meegegeven om gebruikers toe te laten om deze aan te passen.  (Deze link zou eigenlijk ook aanpasbaar moeten zijn in de functies die deze aanroepen!)
#'
#' @param path Het path waarlangs de databank toegankelijk is (inclusief databanknaam)
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
#' @importFrom RODBC odbcConnectAccess2007
#' @importFrom assertthat assert_that is.string
#' @importFrom utils file_test
#'

connecteerMetLSVIdbLokaal <- function(path = "C://Users/els.lommelen@inbo.be/els.lommelen@inbo.be/10194_Controlelijsten/Output/LSVIdb_v4.accdb"){
  assert_that(is.string(path))
  if (!file_test("-f", path)) {
    stop("File does not exists")
  }
  odbcConnectAccess2007(path)
}
