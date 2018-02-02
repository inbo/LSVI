#' Berekent de voorwaarde op basis van datasets
#' 
#' Deze hulpfunctie berekent de waarde voor een opgegeven voorwaarde (verwijzend naar de 'rekenregels' in de LSVI-databank) op basis van opgegeven datasets.  Ze doet dit voor 1 enkele voorwaarde en 1 enkele opname (datum + locatie).  Deze functie test NIET of de datasets zich beperken tot een enkele opname, dus het is aan de gebruiker om enkel gegevens van 1 locatie mee te geven.  Voor een berekening van meerdere opnamen (en ook de volledige LSVI i.p.v. enkel 1 voorwaarde) verwijzen we naar de functie berekenLSVIbasis.
#' 
#' @param VoorwaardeID ID-nummer (uit databank) van de voorwaarde die moet berekend worden
#' @param Kenmerken Dataframe met soorten of kenmerken en hun bedekking voor 1 opname met minimum de velden ...
#' @param ConnectieLSVIhabitats
#' @param LIJST
#' 
#' @return Meestal 1 waarde die het resultaat is van de berekening.  Ingeval de opgegeven datasets data bevatten die als interval geïnterpreteerd worden (bv. bedekkingen opgegeven als Tansley-categorieën), dan wordt een minimum en maximum doorgegeven als een vector met 2 getallen.
#' 
#' @importFrom RODBC sqlQuery
#' 
#' @export

berekenVoorwaarde <-
  function(
    VoorwaardeID,
    Kenmerken,
    ConnectieLSVIhabitats,
    LIJST
  ) {
    
    AV <- analyseVariabele_c(VoorwaardeID, Kenmerken, ConnectieLSVIhabitats, LIJST)
    
    return(berekenWaarde(AV))
    
  }
