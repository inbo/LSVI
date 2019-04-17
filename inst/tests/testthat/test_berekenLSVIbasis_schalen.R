context("test berekenLSVIbasis: gebruik van schalen")

library(readr)
library(dplyr)
library(rlang)

maakConnectiepoolSQLite()
Data_habitat <-
  read_csv2(
    system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
    col_types = list(col_character(), col_character(), col_character())
  )
Data_voorwaarden <-
  read_csv2(
    system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI")
  )
Data_soortenKenmerken <-
  read_csv2(
    system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
  )

load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))

describe("invoer van lokale schaal", {
  it("lokale schaal wordt herkend en omzetting/berekening gebeurt correct", {
    Data_voorwaardenLokaal <- Data_voorwaarden %>%
      mutate(
        Waarde = ifelse(.data$Waarde == "o", "lf", .data$Waarde)
      )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaardenLokaal,
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Waarde = ifelse(.data$Waarde == "o", "lf", .data$Waarde)
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]]
      )
    )
  })
})

library(pool)
poolClose(ConnectiePool)
