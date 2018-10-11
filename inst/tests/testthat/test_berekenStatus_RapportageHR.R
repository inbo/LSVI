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

# Resultaat_RapportageHR <-
#   idsWissen(
#     berekenLSVIbasis(
#       Versie = "Versie 3",
#       Kwaliteitsniveau = "1", Data_habitat,
#       Data_voorwaarden, Data_soortenKenmerken,
#       Aggregatiemethode = "RapportageHR"        #nolint
#     )
#   )
#
# save(Resultaat_RapportageHR, file = "inst/vbdata/Resultaat_RapportageHR_test.Rdata")  #nolint
# load("inst/vbdata/Resultaat_RapportageHR_test.Rdata")  #nolint

load(system.file("vbdata/Resultaat_RapportageHR_test.Rdata", package = "LSVI"))

describe("bereken status criterium en globaal volgens Rapportage HR", {
  it("Status Rapportage HR correct berekend", {
    skip_on_cran()
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          ConnectieLSVIhabitats = ConnectieLSVIhabitats,
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken,
          Aggregatiemethode = "RapportageHR"
        )
      ),
      Resultaat_RapportageHR
    )
  })
})
