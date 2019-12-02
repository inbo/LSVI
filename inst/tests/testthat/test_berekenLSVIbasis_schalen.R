context("test berekenLSVIbasis: gebruik van schalen")

library(readr)
library(dplyr)
library(rlang)

maakConnectiepoolSQLite()
Data_habitat <- #nolint
  read_csv2(
    system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
    col_types = list(col_character(), col_character(), col_character())
  )
Data_voorwaarden <- #nolint
  read_csv2(
    system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI")
  )
Data_soortenKenmerken <- #nolint
  read_csv2(
    system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
  )

load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))

describe("Afhandeling van lokale schaal gebeurt correct", {
  it("lokale schaal wordt herkend en omzetting/berekening gebeurt correct", {
    expect_equal(
      (idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Waarde =
                ifelse(
                  .data$Indicator == "dwergstruiken" & .data$Waarde == "f",
                  "la",
                  .data$Waarde
                )
            ),
          Data_soortenKenmerken
        )
      ))[["Resultaat_detail"]],
      Resultaatv2[["Resultaat_detail"]] %>%
        mutate(
          Waarde =
            ifelse(
              .data$ID == "JR0216" & .data$Indicator == "dwergstruiken",
              "la",
              .data$Waarde
            ),
          Verschilscore =
            ifelse(
              .data$ID == "JR0216" & .data$Indicator == "dwergstruiken",
              -0.824,
              .data$Verschilscore
            )
        )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            mutate(
              Waarde =
                ifelse(
                  .data$Waarde == "f",
                  "la",
                  .data$Waarde
                )
            )
        )
      ),
      Resultaatv2
    )
  })
})

library(pool)
poolClose(ConnectiePool)
