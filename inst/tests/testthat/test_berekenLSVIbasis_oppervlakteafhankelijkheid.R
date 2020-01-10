context("test berekenLSVIbasis oppervlakte-afhankelijkheid")

library(dplyr)
library(readr)

maakConnectiePool()

describe("oppervlakte-afhankelijk", {
  it("referentiewaarden aantal sleutelsoorten is oppervlakte-afhankelijk", {
    Data_habitat <- #nolint
      data.frame(
        ID = 1,
        Habitattype = "6510_hu",
        Opp_m2 = 9,
        stringsAsFactors = FALSE)
    Data_soortenKenmerken <- #nolint
      data.frame(
        ID = 1,
        Kenmerk = c("Lathyrus tuberosus", "Knautia arvensis",
                    "Geranium pratense", "Briza media", "Tragopogon pratensis"),
        TypeKenmerk = "soort_Latijn",
        Waarde = c(10, 20, 5, 10, 5),
        Type = "Percentage",
        Invoertype = NA,
        Eenheid = "%",
        Vegetatielaag = "kruidlaag",
        stringsAsFactors = FALSE)
    Resultaat <- #nolint
      data.frame(
        ID = "1",
        Habitattype = "6510_hu",
        Opp_m2 = 9,
        Versie = "Versie 3",
        Habitattype.y = "6510",
        Criterium = "Vegetatie",
        Indicator = "sleutelsoorten",
        Beoordeling =
          "B: 7-9",
        Kwaliteitsniveau = 1,
        Belang = "b",
        Voorwaarde =
          "aantal sleutelsoorten",
        Referentiewaarde = "4",
        Operator = ">=",
        EenheidRefwaarde = as.character(NA),
        TypeRefwaarde = "Geheel getal",
        InvoertypeRevwaarde = as.character(NA),
        Waarde = 5,
        TypeWaarde = "Geheel getal",
        AfkomstWaarde = "berekend",
        TheoretischMaximum = 58,
        InvoertypeWaarde = as.character(NA),
        EenheidWaarde = as.character(NA),
        Status_voorwaarde = FALSE,
        Verschilscore = (5 - 4) / (8 - 4),
        stringsAsFactors = FALSE
      )
    resultaat_berekend <- #nolint
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat,
          Data_soortenKenmerken = Data_soortenKenmerken,
          na.rm = TRUE,
          Oppervlakte_opname = TRUE
        )
      )
    expect_equal(
      resultaat_berekend[["Resultaat_detail"]] %>%
        filter(.data$Indicator == "sleutelsoorten"),
      Resultaat
    )

  })
})
