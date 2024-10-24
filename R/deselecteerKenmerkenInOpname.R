#' @title Controle van de ingevoerde opname
#'
#' @description Deze hulpfunctie voor de s4-klassen 'aantal' en 'bedekking'
#' selecteert soorten of kenmerken uit een opname die niet tot de soortgroep of
#' studiegroep van een bepaalde voorwaarde behoren.  Op basis hiervan kan de
#' s4-klassen maxBedekkingExcl berkend worden (bv. dominantie van een soort:
#' maximale bedekking van soorten in een opname exclusief de sleutelsoorten).
#'
#'
#' @param Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk,
#' TypeKenmerk, WaardeMin en WaardeMax
#' @param Soortengroep dataframe met de soortenlijst die uit Kenmerken
#' gedeselecteerd moet worden
#' @param Studiegroep dataframe met de lijst kenmerken die uit Kenmerken
#' gedeselecteerd moet worden
#' @param SubAnalyseVariabele heeft waarde 'bedekking' als er een subvoorwaarde
#' is voor de bedekking van de geselecteerde soorten of kenmerken
#' @param SubRefMin minimumwaarde van de grenswaarde voor de bedekking
#' @param SubRefMax maximumwaarde van de grenswaarde voor de bedekking
#' @param SubOperator operator voor deze subvoorwaarde: moet de bedekking hoger
#' of lager liggen dan de opgegeven referentiewaarde?
#'
#' @return Deze functie geeft een aangepaste tabel Data_soorten terug waarin
#' enkel de soorten uit de soortenlijst(en) opgenomen zijn en die bovendien
#' gekoppeld is aan de gegevens van de soortenlijst.
#'
#'
#' @export
#'
#' @importFrom dplyr %>% filter anti_join left_join mutate distinct
#' @importFrom rlang .data
#'
#'
deselecteerKenmerkenInOpname <-
  function(
    Kenmerken,
    Soortengroep,
    Studiegroep,
    SubAnalyseVariabele,
    SubRefMin,
    SubRefMax,
    SubOperator
  ) {

    if (length(Kenmerken) == 0) {
      return(NA)
    }

    if (length(Soortengroep) > 0) {
      Resultaat <- Kenmerken %>%
        filter(tolower(.data$TypeKenmerk) == "soort_nbn") %>%
        anti_join(
          Soortengroep,
          by = c("Kenmerk" = "NbnTaxonVersionKey")
        )
    }

    if (length(Studiegroep) > 0) {

      Resultaat <- Kenmerken %>%
        filter(.data$TypeKenmerk == "studiegroep") %>%
        anti_join(
          Studiegroep,
          by = c("Kenmerk" = "Waarde")
        )
    }

    if (!exists("Resultaat")) {
      stop("Er ontbreekt een soortenlijst of studiegroeplijst in de databank.  Meld deze fout aan de beheerder van dit package.") #nolint
    }

    if (identical(SubAnalyseVariabele, character(0))) {
      Resultaat <- Resultaat %>%
        filter(
          .data$WaardeMax > 0 | (is.na(.data$WaardeMax) & .data$WaardeMin > 0)
        ) %>%
        distinct()

      return(Resultaat)
    } else {
      stop(
        paste(
          "Fout in de indicatorendatabank: een analysevariabele met achtervoegsel 'Excl' bevat een subanalysevariabele en dit wordt niet ondersteund in het script.  Meld deze fout aan de beheerder van dit package."  #nolint
        )
      )
    }

    return(Resultaat)
  }
