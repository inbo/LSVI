context("test bereken Status Rapportage HR")

library(readr)
library(dplyr)
library(rlang)
maakConnectiePool()

Data_habitat <-
    read_csv2(
      system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"))
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
    )

load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))

describe("bereken status criterium en globaal volgens Rapportage HR", {
  it("Status Rapportage HR correct berekend", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken,
          Aggregatiemethode = "RapportageHR"
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Aggregatiemethode = "RapportageHR"
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]],
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Status = ifelse(.data$ID == "Ts2036", TRUE, .data$Status),
            Aggregatiemethode = "RapportageHR"
          )
      )
    )
  })
})

library(pool)
poolClose(ConnectiePool)
