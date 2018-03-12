#' @title Controle van de ingevoerde opname
#'
#' @description Deze hulpfunctie voor de s4-klassen 'aantal' en 'bedekking' zoekt soorten of kenmerken uit de voorwaarde in de opname en maakt een lijstje van de soorten die voldoen en in de opname voorkomen.  Op basis hiervan kunnen de s4-klassen het totale aantal of de bedekking berekenen.
#'
#' 
#' @param Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' @param Soortengroep dataframe met de soortenlijst die uit Kenmerken gehaald moet worden
#' @param Studiegroep dataframe met de lijst kenmerken die uit Kenmerken gehaald moet worden
#' @param SubAnalyseVariabele heeft waarde 'bedekking' als er een subvoorwaarde is voor de bedekking van de geselecteerde soorten of kenmerken
#' @param SubRefMin minimumwaarde van de grenswaarde voor de bedekking
#' @param SubRefMax maximumwaarde van de grenswaarde voor de bedekking
#' @param SubOperator operator voor deze subvoorwaarde: moet de bedekking hoger of lager liggen dan de opgegeven referentiewaarde?
#' 
#' @return Deze functie geeft een aangepaste tabel Data_soorten terug waarin enkel de soorten uit de soortenlijst(en) opgenomen zijn en die bovendien gekoppeld is aan de gegevens van de soortenlijst.
#' 
#'
#' @export   
#'
#' @importFrom dplyr %>% filter left_join inner_join mutate
#' @importFrom RODBC sqlQuery
#'
#'
selecteerKenmerkenInOpname <-
  function(
    Kenmerken,
    Soortengroep,
    Studiegroep,
    SubAnalyseVariabele,
    SubRefMin,
    SubRefMax,
    SubOperator
  ){

    if (length(Kenmerken) == 0) {
      return(NA)
    }

    if (length(Soortengroep) > 0) {
      Resultaat <- Kenmerken %>%
        filter(tolower(.data$TypeKenmerk) == "soort_nbn") %>%
        left_join(
          Soortengroep,
          by = c("Kenmerk" = "NBNTaxonVersionKey")
        )
      #Hier moet nog toegevoegd worden dat subsoorten opgehaald worden als de soorten zelf niet in de kenmerkenlijst staan (zie berekenAantalSoorten en selecteerSoortenInOpname)

    }

    if (length(Studiegroep) > 0) {

      Resultaat <- Kenmerken %>%
        filter(.data$TypeKenmerk == "studiegroep") %>%
        inner_join(
          Studiegroep,
          by = c("Kenmerk" = "Waarde")
        )
    }

    if (!identical(SubAnalyseVariabele, character(0)) &&
        SubAnalyseVariabele == "bedekking") {
      Resultaat <- Resultaat %>%
        mutate(
          RefMin = SubRefMin,
          RefMax = SubRefMax,
          Operator = SubOperator,
          Rijnr = row_number(.data$Kenmerk)
        )

      SubStatusberekening <-
        berekenStatus(
          Resultaat[
            , c(
              "Rijnr",
              "RefMin",
              "RefMax",
              "Operator",
              "WaardeMin",
              "WaardeMax"
            )
          ]
        )

      Resultaat <- Resultaat %>%
        left_join(
          SubStatusberekening,
          by = c("Rijnr")
        ) %>%
        mutate(
          Rijnr = NULL
        ) %>%
        filter(
          .data$Status == TRUE
        )

    } else {
      Resultaat <- Resultaat %>%
        filter(
          .data$WaardeMax > 0
        )
    }

    return(Resultaat)
  }