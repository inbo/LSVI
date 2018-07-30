context("test berekenLSVIbasis vegetatielaag")

library(readr)
library(dplyr)
library(rlang)

Data_habitat <-
    read_csv2(
      system.file("vbdata/Test9190habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(
      system.file("vbdata/Test9190voorwaarden.csv", package = "LSVI"),
      col_types =
        list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
    ) %>%
  mutate(  #idee is om onderstaande code te wissen zodra de rekenmodule hiervoor aangepast is
    Eenheid =
      ifelse(
        .data$Eenheid == "ha",
        NA,
        .data$Eenheid
      )
  ) %>%
  bind_rows(
    data.frame(
      ID = "1", Criterium = "Vegetatie",
      Indicator = "sleutelsoorten van de kruidlaag",
      Voorwaarde = "aandeel sleutelsoorten kruidlaag", Waarde = "20",
      Type = "Geheel getal",
      stringsAsFactors = FALSE
    )
  )
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
      col_types =
        list(col_character(), col_character(), col_character(), col_character(),
             col_character(), col_character(), col_character(), col_character())
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
# save(Resultaat, file = "inst/vbdata/Resultaat_test_bos.Rdata")
# load("inst/vbdata/Resultaat_test_bos.Rdata")

load(system.file("vbdata/Resultaat_test_bos.Rdata", package = "LSVI"))

describe("berekenLSVIbasis vegetatielaag", {
  it("de vegetatielagen worden correct geselecteerd", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1", 
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1", 
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken =
            Data_soortenKenmerken %>%
              mutate(
                Vegetatielaag =
                  ifelse(
                    Vegetatielaag == "struiklaag",
                    "boomlaag",
                    Vegetatielaag
                  )
              )
        )
      ),
      Resultaat
    )
    BerekendRes <-
      idsWissen(
        berekenLSVIbasis(
            Versie = "Versie 3", Kwaliteitsniveau = "1", 
            Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
            Data_soortenKenmerken =
              Data_soortenKenmerken %>%
              mutate(
                Vegetatielaag =
                  ifelse(
                    Vegetatielaag == "struiklaag",
                    "kruidlaag",
                    Vegetatielaag
                  )
              )
          )
      )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_criterium"]],
        Resultaat[["Resultaat_criterium"]]
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_indicator"]],
        Resultaat[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Indicator ==
                  "sleutelsoorten van de boom- en struiklaag",
                -0.5714285714,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_detail"]],
        Resultaat[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Voorwaarde ==
                  "grondvlak sleutelsoorten boom- en struiklaag",
                "0.3",
                .data$Waarde
              ),
            Verschilscore =
              ifelse(
                .data$Voorwaarde ==
                  "grondvlak sleutelsoorten boom- en struiklaag",
                -0.5714285714,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_globaal"]],
        Resultaat[["Resultaat_globaal"]]
      )
    )
  })


})
