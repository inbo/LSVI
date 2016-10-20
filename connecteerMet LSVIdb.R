#'
#'@title Voert de opgegeven query uit op de databank met LSVI-indicatoren
#'
#'@description Deze functie voert opgegeven queries uit op de databank met LSVI-indicatoren en geeft de gegenereerde tabel terug.  Bedoeling van deze aparte functie is om de link naar deze databank maar op 1 plaats te bewaren, zodat deze bij het verplaatsen van de databank ook maar op een plaats aangepast moet worden.
#'
#'@param query De query die uitgevoerd moet worden op de t
#'
#'@return de gegenereerde tabel
#'
#'@export
#'
#'@importFrom RODBC odbcConnectAccess2007
#'@importFrom assertthat assert_that
#'

connecteerMetLSVIdb <- function(path = "C://Users/els.lommelen@inbo.be/els.lommelen@inbo.be/10194_Controlelijsten/Output/LSVIdb_v4.accdb"){
  assert_that(is.string(path))
  if (!file_test("-f", path)) {
    stop("File does not exists")
  }
  odbcConnectAccess2007(path)
}
