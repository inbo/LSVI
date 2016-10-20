#'
#'@title Geef alle unieke waarden van een veld uit de databank met de LSVI-indicatoren voorafgegaan door 'alle'
#'
#'@description Deze functie geeft een vector met alle verschillende waarden die een gespecifieerd veld van een gespecifieerde tabel in de databank met LSVI-indicatoren staan, voorafgegaan door de (toegevoegde) waarde 'alle'.  Deze functie wordt in verschillende functies van het package gebruikt om de invoer van parameters te controleren (waar de mogelijke invoer bestaat uit 'alle' of een item uit het veld).
#'
#'@param Tabelnaam De naam van de tabel waarin het veld zich bevindt (String)
#'
#'@param Veldnaam De naam van het veld (in de bij Tabelnaam opgegeven tabel) waarvan de waarden moeten opgezocht worden
#'
#'@return vector bestaande uit 'alle' en de verschillende waarden uit de gespecifieerde tabel
#'
#'@export
#'
#'@importFrom RODBC sqlQuery odbcClose
#'

geefUniekeWaarden <- function(Tabelnaam, Veldnaam){
  query <- sprintf("SELECT %s FROM %s",Veldnaam, Tabelnaam)
  connectie <- connecteerMetLSVIdb()
  Waarden <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
  odbcClose(connectie)
  UniekeWaarden <- c("alle", Waarden[,Veldnaam])
  return(UniekeWaarden)
}