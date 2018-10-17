context("test bereken Status Rapportage HR")

library(readr)
library(dplyr)
library(rlang)

Data_habitat <-
    read_csv2(
      system.file("vbdata/opname4030habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(system.file("vbdata/opname4030voorwaarden.csv", package = "LSVI"))
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/opname4030soortenKenmerken.csv", package = "LSVI")
    )

load(system.file("vbdata/Resultaat_test4030.Rdata", package = "LSVI"))

describe("bereken status criterium en globaal volgens Rapportage HR", {
  it("Status Rapportage HR correct berekend", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken,
          Aggregatiemethode = "RapportageHR"
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]] %>%
          mutate(
            Aggregatiemethode = "RapportageHR"
          ),
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail = Resultaat[["Resultaat_detail"]],
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]] %>%
          mutate(
            Status = ifelse(.data$ID == "Ts2036", TRUE, .data$Status),
            Aggregatiemethode = "RapportageHR"
          )
      )
    )
  })
})
