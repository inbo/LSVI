context("test speciale gevallen")

library(dplyr)
library(readr)

maakConnectiePool()
describe("twee voorwaarden vergelijken", {
  it("vergelijking wordt correct uitgevoerd", {
    Data_habitat <- #nolint
      data.frame(
        ID = 1, Habitattype = "1330_hpr",
        stringsAsFactors = FALSE)
    Data_voorwaarden <- #nolint
      data.frame(
        ID = 1, Criterium = "Verstoring", Indicator = "overgang naar rbbzil",
        Voorwaarde = c("bedekking grasachtigen rbbzil",
                       "som van de bedekking sleutelsoorten"),
        Waarde = c(10, 20), Type = "Percentage", Invoertype = NA, Eenheid = "%",
        stringsAsFactors = FALSE)
    Data_soortenKenmerken <- #nolint
      data.frame(
        ID = 1, Kenmerk = c("Carex hirta", "Carex distans"),
        TypeKenmerk = "soort_Latijn", Waarde = c(10, 20), Type = "Percentage",
        Invoertype = NA, Eenheid = "%", Vegetatielaag = NA,
        stringsAsFactors = FALSE)
    Resultaat <-
      data.frame(
        ID = "1",
        Habitattype = "1330_hpr",
        Versie = "Versie 2.0",
        Habitattype.y = "1330",
        Criterium = "Verstoring",
        Indicator = "overgang naar rbbzil",
        Beoordeling =
          "B: som van de bedekking grasachtigen uit het zilverschoonverbond <= som van de bedekking sleutelsoorten", #nolint
        Kwaliteitsniveau = 1,
        Belang = "zb",
        Voorwaarde =
          "bedekking grasachtigen rbbzil <= som van de bedekking sleutelsoorten", #nolint
        Referentiewaarde = "20",
        Operator = "<=",
        EenheidRefwaarde = "%",
        TypeRefwaarde = "Percentage",
        InvoertypeRevwaarde = as.character(NA),
        Waarde = "10",
        TypeWaarde = "Percentage",
        InvoertypeWaarde = as.character(NA),
        EenheidWaarde = "%",
        AfkomstWaarde = "observatie",
        TheoretischMaximum = 100,
        Status_voorwaarde = TRUE,
        Verschilscore = 0.5,
        stringsAsFactors = FALSE
      )
    WarningMicrorelief <-
      "De waarde\\(n\\) voor de voorwaarde\\(n\\) goed ontwikkelde structuur, microreliëf aanwezig \\(VoorwaardeID 2205, 2369\\) kunnen niet berekend worden voor opname\\(n\\) 1. Geef de waarde voor deze voorwaarde rechtstreeks in als input van de functie 'berekenLSVIBasis' via tabel 'Data_voorwaarden' \\(zie \\?berekenLSVIbasis voor meer info\\). Vermeld hierbij Criterium = Structuur, Indicator = horizontale structuur, microreliëf en Voorwaarde = goed ontwikkelde structuur, microreliëf aanwezig." #nolint: line_length_linter
    expect_warning(
      resultaat_berekend <-
        idsWissen(
          berekenLSVIbasis(
            Versie = "Versie 2.0",
            Kwaliteitsniveau = "1",
            Data_habitat,
            Data_voorwaarden
          )
        ),
      WarningMicrorelief
    )
    expect_equal(
      resultaat_berekend[["Resultaat_detail"]] %>%
        filter(.data$Indicator == "overgang naar rbbzil"),
      Resultaat
    )
    expect_equal(
      as.data.frame(
        resultaat_berekend[["Resultaat_indicator"]] %>%
          filter(.data$Indicator == "overgang naar rbbzil")
      ),
      Resultaat %>%
        select(
          ID, Habitattype, Versie, Habitattype.y, Criterium, Indicator,
          Beoordeling, Belang, Kwaliteitsniveau
        ) %>%
        mutate(
          Kwaliteitsniveau = as.integer(Kwaliteitsniveau),
          Status_indicator = TRUE,
          Verschilscore = 0.5
        )
    )
    Resultaat2 <- Resultaat %>%
      mutate(
        AfkomstWaarde = "berekend"
      )
    Resultaat2 <-
      Resultaat2[
        shuffle_columns(names(Resultaat2), "AfkomstWaarde before EenheidWaarde")
      ]
    Resultaat2 <-
      Resultaat2[
        shuffle_columns(
          names(Resultaat2),
          "AfkomstWaarde before InvoertypeWaarde"
        )
      ]
    Resultaat2 <-
      Resultaat2[
        shuffle_columns(
          names(Resultaat2),
          "TheoretischMaximum before EenheidWaarde"
        )
      ]
    Resultaat2 <-
      Resultaat2[
        shuffle_columns(
          names(Resultaat2),
          "TheoretischMaximum before InvoertypeWaarde"
        )
      ]
    expect_warning(
      TestResultaat <- idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      WarningMicrorelief
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] %>%
        filter(.data$Indicator == "overgang naar rbbzil"),
      Resultaat2
    )
  })
})

describe("AnalyseVariabele scoresom", {
  Data_habitat <- #nolint
    data.frame(
      ID = c("demo1", "demo2", "demo3"),
      Habitattype = "2190_a",
      stringsAsFactors = FALSE)
  Data_voorwaarden <- #nolint
    read_csv2(
      system.file("vbdata/data_voorwaarden2190_a.csv", package = "LSVI")
    )
  Data_soortenKenmerken <- #nolint
    read_csv2(
      system.file("vbdata/data_soortenKenmerken2190_a.csv", package = "LSVI")
    )
  it("berekening op basis van soortenlijst geeft correct resultaat", {
    resultaat_berekend <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      )
    expect_equal(
      (resultaat_berekend[["Resultaat_detail"]] %>%
        filter(.data$Indicator == "bedekking sleutelsoorten"))$Waarde,
      c("6", "3", "2", "1", "8", "1")
    )
    expect_equal(
      (resultaat_berekend[["Resultaat_detail"]] %>%
         filter(.data$Indicator == "bedekking sleutelsoorten"))$Verschilscore,
      c(0, 0, -0.666666666667, -0.66666666667, 0.2222222222, -0.666666666667)
    )
    expect_equal(
      as.data.frame(
        resultaat_berekend[["Resultaat_indicator"]] %>%
          filter(.data$Indicator == "bedekking sleutelsoorten")
      )$Verschilscore,
      c(0, -0.666666666667, 0.22222222222)
    )
    expect_equal(
      as.data.frame(
        resultaat_berekend[["Resultaat_indicator"]] %>%
          filter(.data$Indicator == "bedekking sleutelsoorten")
      )$Status_indicator,
      c(TRUE, FALSE, TRUE)
    )
  })
  it("opgeven van voorwaarde geeft correct resultaat", {
    Data_voorwaarden <- Data_voorwaarden %>%
      bind_rows(
        data.frame(
          ID = c("demo1", "demo1", "demo2", "demo2", "demo3", "demo3"),
          Criterium = "Vegetatie",
          Indicator = "bedekking sleutelsoorten",
          Voorwaarde = c("scoresom hydrofyten", "scoresom niet-hydrofyten"),
          Waarde = c(7, 4, 3, 2, 7, 2),
          Type = "Geheel getal"
        )
      )
    resultaat_berekend <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      )
    expect_equal(
      (resultaat_berekend[["Resultaat_detail"]] %>%
         filter(.data$Indicator == "bedekking sleutelsoorten"))$Waarde,
      c("7", "4", "3", "2", "7", "2")
    )
  })
})

library(pool)
poolClose(ConnectiePool)
