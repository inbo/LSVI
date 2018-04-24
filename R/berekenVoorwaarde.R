#' Berekent de voorwaarde op basis van datasets
#' 
#' Deze hulpfunctie berekent de waarde voor een opgegeven voorwaarde (verwijzend naar de 'rekenregels' in de LSVI-indicatorendatabank) op basis van opgegeven datasets.  Ze doet dit voor 1 enkele voorwaarde en 1 enkele opname (datum + locatie).  Deze functie test NIET of de datasets zich beperken tot een enkele opname, dus het is aan de gebruiker om enkel gegevens van 1 locatie mee te geven.  Voor een berekening van meerdere opnamen (en ook de volledige LSVI i.p.v. enkel 1 voorwaarde) verwijzen we naar de functie berekenLSVIbasis.
#' 
#' @inheritParams berekenLSVIbasis
#' @param OpnameID nummer van de opname
#' @param VoorwaardeID ID-nummer (uit LSVI-indicatorendatabank) van de voorwaarde die moet berekend worden
#' @param Kenmerken Dataframe met soorten of kenmerken en hun bedekking voor 1 opname met minimum de velden ID, Kenmerk, TypeKenmerk, Waarde, Type, Invoertype en Eenheid (data identiek aan Data_soortenKenmerken in berekenLSVIbasis)
#' 
#' @return Een vector van 2 waarden die het resultaat is van de berekening, namelijk het minimum en het maximum van het interval waartussen de berekende waarde ligt.  Als het resultaat een exacte waarde is en geen interval, bevat de vector tweemaal dezelfde waarde.  (Het resultaat is een interval als de brondata categorische variabelen zijn, bv. bedekkingen volgens de beheermonitoringschaal of Tansley-schaal.)
#' 
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @export

berekenVoorwaarde <-
  function(
    OpnameID,
    VoorwaardeID,
    Kenmerken,
    ConnectieLSVIhabitats,
    LIJST
  ) {

    KenmerkenID <- Kenmerken %>%
      filter(.data$ID == OpnameID)

    AV <-
      analyseVariabele_c(
        VoorwaardeID,
        KenmerkenID,
        ConnectieLSVIhabitats,
        LIJST
      )

    Waarde <- berekenWaarde(AV)

    if (length(Waarde) == 1) {
      Waarde <- c(Waarde, Waarde)
    }

    #Aan de waarde als 3de item het theoretisch maximum van de voorwaarde
    #toevoegen
    Waarden <-
      list(
        Min = Waarde[1],
        Max = Waarde[2],
        TheoretischMaximum = geefTheoretischMaximum(AV)
      )

    return(Waarden)
  }
