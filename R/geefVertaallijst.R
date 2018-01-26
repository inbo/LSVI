#' Haalt vertaallijst op uit databank
#' 
#' @importFrom RODBC sqlQuery odbcClose
#' 
#' @export

geefVertaallijst <- function(ConnectieLSVIhabitats) {
  Connectie <- connecteerMetLSVIdb()
  query <- 
    "SELECT Lijst.Naam, LijstItem.Waarde, LijstItem.Volgnummer, LijstItem.Omschrijving, LijstItem.Ondergrens,
    LijstItem.Gemiddelde, LijstItem.Bovengrens
    FROM LijstItem INNER JOIN Lijst ON LijstItem.LijstId = Lijst.Id"
  LIJST <- RODBC::sqlQuery(Connectie, query, stringsAsFactors = FALSE)
  RODBC::odbcClose(Connectie)
  
  return(LIJST)
}




