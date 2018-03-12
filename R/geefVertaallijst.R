#' Haalt vertaallijst op uit databank
#' 
#' @description Deze functie haalt een lijst uit de databank van de gebruikte schalen (bv. Tansley-schaal en beheermonitoringschaal uit 2017) met telkens de verschillende mogelijke categorieën en een overeenkomstige bedekkingswaarde.  De functie berekenLSVIbasis gebruikt deze waarden om de nodige omzettingen te doen tussen deze verschillende schalen
#' 
#' @inheritParams berekenLSVIbasis
#' 
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% mutate
#' 
#' @return Dataframe met Naam, Waarde, Volgnummer, Omschrijving, Ondergrens, Gemiddelde en Bovengrens. Telkens is een waarde tussen 0 en 1 opgegeven die afkomstig is van het delen van het percentage door 100)
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
