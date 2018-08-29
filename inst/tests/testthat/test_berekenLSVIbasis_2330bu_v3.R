context("test bereken waarde voor voorwaarden van 2330_bu versie 3")

library(readr)
library(dplyr)
library(rlang)

Data_habitat <-
    read_csv2(
      system.file("vbdata/data_habitat2330_bu.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(system.file("vbdata/data_voorwaarden2330_bu.csv", package = "LSVI"))
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/data_soortenKenmerken2330_bu.csv", package = "LSVI")
    )

Resultaat <- read_csv2(system.file("vbdata/resultaat2330_bu.csv", package = "LSVI"))

describe("berekenLSVIbasis 2330_bu versie 3", {

  it("waarden van voorwaarden correct berekend", {
    skip_on_cran()

    resultaat_berekend <- berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        ) %>%
      select(Habitattype, Versie, Criterium, Indicator, Voorwaarde, Waarde)

    expect_equal(
      resultaat_berekend,
      Resultaat
    )
  })
})

