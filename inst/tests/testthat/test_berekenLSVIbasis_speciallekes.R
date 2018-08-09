context("test speciale gevallen")

library(dplyr)

describe("twee voorwaarden vergelijken", {
  it("vergelijking wordt correct uitgevoerd", {
    Data_habitat <-
      data.frame(
        ID = 1, Habitattype = "1330_hpr",
        stringsAsFactors = FALSE)
    Data_voorwaarden <-
      data.frame(
        ID = 1, Criterium = "Verstoring", Indicator = "overgang naar rbbzil",
        Voorwaarde = c("som bedekking grasachtigen uit het zilverschoonver",
                       "som van de bedekking sleutelsoorten"),
        Waarde = c(10, 20), Type = "Percentage", Invoertype = NA, Eenheid = "%",
        stringsAsFactors = FALSE)
    Data_soortenKenmerken <-
      data.frame(
        ID = 1, Kenmerk = c("Carex hirta", "Carex distans"),
        TypeKenmerk = "soort_Latijn", Waarde = c(10, 20), Type = "Percentage",
        Invoertype = NA, Eenheid = "%", Vegetatielaag = NA,
        stringsAsFactors = FALSE)
    Resultaat <-
      data.frame(
        ID = "1",
        Habitattype = "1330_hpr",
        Versie = "Versie 3",
        Habitattype.y = "1330",
        Criterium = "Verstoring",
        Indicator = "overgang naar rbbzil",
        Beoordeling =
          "som van de bedekking grasachtigen uit het zilverschoonverbond <= som van de bedekking sleutelsoorten", #nolint
        Kwaliteitsniveau = 1,
        Voorwaarde =
          "som bedekking grasachtigen uit het zilverschoonver <= som van de bedekking sleutelsoorten", #nolint
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
        Status_voorwaarde = TRUE,
        stringsAsFactors = FALSE
      )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden
        )
      )[["Resultaat_detail"]] %>%
        filter(.data$Indicator == "overgang naar rbbzil"),
      Resultaat
    )
    Resultaat2 <- Resultaat %>%
      mutate(
        AfkomstWaarde = "berekend"
      )
    Resultaat2 <-
      Resultaat2[shuffle_columns(names(Resultaat2), "AfkomstWaarde before EenheidWaarde")]
    Resultaat2 <-
      Resultaat2[shuffle_columns(names(Resultaat2), "AfkomstWaarde before InvoertypeWaarde")]
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      )[["Resultaat_detail"]] %>%
        filter(.data$Indicator == "overgang naar rbbzil"),
      Resultaat2
    )
  })
})
