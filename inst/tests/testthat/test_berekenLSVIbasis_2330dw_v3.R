context("test berekening 2330dw")

library(readr)
library(dplyr)
library(rlang)

maakConnectiePool()
Data_habitat <-
    read_csv2(
      system.file("vbdata/data_habitat2330_dw.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(
      system.file("vbdata/data_voorwaarden2330_dw.csv", package = "LSVI")
    )
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/data_soortenKenmerken2330_dw.csv", package = "LSVI")
    )

# Resultaat <-
#   idsWissen(
#     berekenLSVIbasis(
#       Versie = "Versie 3",
#       Kwaliteitsniveau = "1", Data_habitat,
#       Data_voorwaarden, Data_soortenKenmerken
#     )
#   )
# 
# save(Resultaat, file = "inst/vbdata/Resultaat_test2330_dw.Rdata")  #nolint
# load("inst/vbdata/Resultaat_test2330_dw.Rdata")  #nolint

load(system.file("vbdata/Resultaat_test2330_dw.Rdata", package = "LSVI"))


describe("eenjarigen + open zand > meerjarigen", {
  it("berekening op basis van voorwaarden", {
    skip_if_not(
      class(ConnectiePool$.__enclos_env__$private$createObject())[1] ==
        "Microsoft SQL Server",
      "SQL Server niet beschikbaar"
    )
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
    skip_if_not(
      class(ConnectiePool$.__enclos_env__$private$createObject())[1] ==
        "Microsoft SQL Server",
      "SQL Server niet beschikbaar"
    )
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
            Index_min_harm = -0.53630858,
            Index_harm_harm = -0.289069178869264
          )
      )
    )
  })
})
library(pool)
poolClose(ConnectiePool)