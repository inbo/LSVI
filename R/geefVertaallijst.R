#' Haalt vertaallijst op uit databank
#' 
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% mutate
#' 
#' @export

geefVertaallijst <-
  function(ConnectieLSVIhabitats) {
  Connectie <- connecteerMetLSVIdb()
  query <-
    "SELECT Lijst.Naam, LijstItem.Waarde, LijstItem.Volgnummer,
    LijstItem.Omschrijving, LijstItem.Ondergrens,
    LijstItem.Gemiddelde, LijstItem.Bovengrens
    FROM LijstItem INNER JOIN Lijst ON LijstItem.LijstId = Lijst.Id"
  LIJST <-
    sqlQuery(Connectie, query, stringsAsFactors = FALSE) %>%
    mutate(
      Ondergrens = .data$Ondergrens / 100,
      Gemiddelde = .data$Gemiddelde / 100,
      Bovengrens = .data$Bovengrens / 100
    )
  odbcClose(Connectie)

  return(LIJST)
}
