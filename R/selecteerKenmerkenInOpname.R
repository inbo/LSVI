#' @title Controle van de ingevoerde opname
#'
#' @description Deze hulpfunctie voor de s4-klassen 'aantal' en 'bedekking' zoekt soorten of kenmerken uit de voorwaarde in de opname en maakt een lijstje van de soorten die voldoen en in de opname voorkomen.  Op basis hiervan kunnen de s4-klassen het totale aantal of de bedekking berekenen.
#'
#'
#' @param Kenmerken dataframe met alle opgegeven kenmerken, met velden Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
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
#' @importFrom dplyr %>% filter left_join inner_join mutate distinct group_by do ungroup
#' @importFrom rlang .data
#' @importFrom stringr str_c
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
        filter(tolower(.data$TypeKenmerk) == "soort_nbn")
      if (nrow(Resultaat) == 0) {
        warning(
          "Er is geen enkele soort opgegeven voor de opname, dus er wordt van uitgegaan dat er geen vegetatie-opname gemaakt is en berekeningen op basis van soortenlijsten zullen resulteren in NA (not available). Geef tenminste 1 soort op (evt. met bedekking 0 %) als er toch een opname gemaakt is."  #nolint
        )
        return(NA)
      }
      Resultaat <- Resultaat %>%
        inner_join(
          Soortengroep,
          by = c("Kenmerk" = "NbnTaxonVersionKey")
        )
      if (length(Studiegroep) > 0 & nrow(Resultaat) > 0) {
        if (max(is.na(Resultaat$Vegetatielaag))) {
          stop(
            "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld, waardoor de berekening niet correct kan worden uitgevoerd (dit omdat de vegetatielaag bepaalt of de betreffende soort al dan niet in rekening gebracht moet worden voor het berekenen van de indicator)"  #nolint
          )
        } else {
          Resultaat <- Resultaat %>%
            filter(
              tolower(.data$Vegetatielaag) %in% tolower(Studiegroep$Waarde)
            )
        }
      }

      #Als het hogere taxonniveau in de opname zit, wordt het lagere gewist
      kiesTaxonOfSubtaxons <-
        function(Dataset) {
          TaxonData <- Dataset %>%
            filter(.data$TaxonId == .data$SubTaxonId)
          if (nrow(TaxonData) < 1) {
            Dataset <- Dataset %>%
              group_by(.data$TaxonId) %>%
              summarise(
                Kenmerk = str_c(.data$Kenmerk, collapse = " & "),
                TypeKenmerk = unique(.data$TypeKenmerk),
                WaardeMax = 1.0 - prod( (1.0 - .data$WaardeMax), na.rm = TRUE),
                WaardeMin = 1.0 - prod( (1.0 - .data$WaardeMin), na.rm = TRUE),
                SubTaxonId = mean(.data$TaxonId)
              )
            return(Dataset)
          }
          return(TaxonData)
        }

      Resultaat <- Resultaat %>%
        group_by(.data$TaxonId) %>%
        do(kiesTaxonOfSubtaxons(.)) %>%
        ungroup()
    }

    if (length(Studiegroep) > 0 & !(length(Soortengroep) > 0)) {
      Resultaat <- Kenmerken %>%
        filter(tolower(.data$TypeKenmerk) == "studiegroep") %>%
        mutate(
          Kenmerk = tolower(.data$Kenmerk)
        ) %>%
        inner_join(
          Studiegroep %>%
            mutate(
              Waarde = tolower(.data$Waarde)
            ),
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
    }

    if (!identical(SubAnalyseVariabele, character(0)) &
        SubAnalyseVariabele == "bedekking") {

      Resultaat <- Resultaat %>%
        mutate(
          RefMin = SubRefMin,
          RefMax = SubRefMax,
          Operator = SubOperator,
          Rijnr = row_number(.data$Kenmerk)
        )

      if (nrow(Resultaat) > 0) {

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
          ) %>%
          distinct()

      }

    } else {
      stop(
        paste(
          "Onbekende subanalysevariabele",
          SubAnalyseVariabele,
          "in de indicatorendatabank.  Meld deze fout aan de beheerder van dit package."  #nolint
        )
      )
    }


    return(Resultaat)
  }
