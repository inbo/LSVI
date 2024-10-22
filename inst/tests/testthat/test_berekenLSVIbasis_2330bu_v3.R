context("test bereken waarde voor voorwaarden van 2330_bu versie 3")

library(readr)
library(dplyr)
library(rlang)

maakConnectiePool()
Data_habitat <- #nolint
    read_csv2(
      system.file("vbdata/data_habitat2330_bu.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <- #nolint
    read_csv2(
      system.file("vbdata/data_voorwaarden2330_bu.csv", package = "LSVI")
    )
Data_soortenKenmerken <- #nolint
    read_csv2(
      system.file("vbdata/data_soortenKenmerken2330_bu.csv", package = "LSVI")
    )

Resultaat <-
  read_csv2(system.file("vbdata/resultaat2330_bu.csv", package = "LSVI"))

describe("berekenLSVIbasis 2330_bu versie 3", {

  it("waarden van voorwaarden correct berekend", {
    expect_warning(
      resultaat_berekend <- berekenLSVIbasis(
            Versie = "Versie 3",
            Kwaliteitsniveau = "1",
            Data_habitat,
            Data_voorwaarden,
            Data_soortenKenmerken
          ),
      "Volgende records uit Data_voorwaarden kunnen niet gekoppeld worden aan indicatoren uit de databank omdat de criterium-indicator-voorwaarde-combinatie niet voorkomt bij de LSVI-regels van het opgegeven habitattype: <55058, Structuur, korstmosvegetaties, bedekking korstmosvegetaties> <55058, Structuur, horizontale structuur, aantal ontwikkelingsstadia>" #nolint: line_length_linter
    )
      

    resultaat_berekend_vw <- resultaat_berekend[[3]] %>%
      select(Habitattype, Versie, Criterium, Indicator, Voorwaarde, Waarde)

    expect_equal(
      resultaat_berekend_vw
        ,
      Resultaat %>%
        select(
          Habitattype, Versie, Criterium, Indicator, Voorwaarde, Waarde
        ) %>%
        mutate(Waarde = as.character(Waarde))
    )
  })
})
library(pool)
poolClose(ConnectiePool)
