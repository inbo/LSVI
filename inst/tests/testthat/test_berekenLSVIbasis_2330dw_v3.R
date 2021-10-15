context("test berekening 2330dw")

library(readr)
library(dplyr)
library(rlang)

maakConnectiePool()
Data_habitat <- #nolint
    read_csv2(
      system.file("vbdata/data_habitat2330_dw.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
attr(Data_habitat, "spec") <- NULL #nolint
Data_voorwaarden <- #nolint
    read_csv2(
      system.file("vbdata/data_voorwaarden2330_dw.csv", package = "LSVI")
    )
Data_soortenKenmerken <- #nolint
    read_csv2(
      system.file("vbdata/data_soortenKenmerken2330_dw.csv", package = "LSVI")
    )

load(system.file("vbdata/Resultaat_test2330_dw.Rdata", package = "LSVI"))


describe("eenjarigen + open zand > meerjarigen", {
  it("berekening op basis van voorwaarden", {
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
      resultaat_berekend,
      Resultaat
    )
  })
  it("berekening op basis van soortenlijst", {
    resultaat_berekend <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            filter(Indicator != "éénjarigen"),
          Data_soortenKenmerken
        )
      )
    stopifnot(
      all.equal(
        resultaat_berekend[["Resultaat_criterium"]],
        Resultaat[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                Criterium == "Structuur",
                0.72137490530303,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                Criterium == "Structuur",
                0.72137490530303,
                .data$Index_harm_criterium
              )
          )
      )
    )
    stopifnot(
      all.equal(
        resultaat_berekend[["Resultaat_indicator"]],
        Resultaat[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                Indicator == "éénjarigen",
                0.72137490530303,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        resultaat_berekend[["Resultaat_detail"]],
        Resultaat[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                Indicator == "éénjarigen",
                "68.4 - 76.4",
                .data$Waarde
              ),
            AfkomstWaarde =
              ifelse(
                Indicator == "éénjarigen",
                "berekend",
                .data$AfkomstWaarde
              ),
            Verschilscore =
              ifelse(
                Indicator == "éénjarigen",
                0.72137490530303,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        resultaat_berekend[["Resultaat_globaal"]],
        Resultaat[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_harm = 0.57294801,
            Index_harm_harm = 0.583875
          )
      )
    )
  })
})
library(pool)
poolClose(ConnectiePool)
