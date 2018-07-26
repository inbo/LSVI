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
      Type = "Geheel getal"
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
#   berekenLSVIbasis(
#     Versie = "Versie 3",
#     Kwaliteitsniveau = "1", Data_habitat,
#     Data_voorwaarden, Data_soortenKenmerken
#   )
# 
# save(Resultaat, file = "inst/vbdata/Resultaat_test_bos.Rdata")
# load("inst/vbdata/Resultaat_test_bos.Rdata")

load(system.file("vbdata/Resultaat_test_bos.Rdata", package = "LSVI"))

describe("berekenLSVIbasis vegetatielaag", {
  it("nog in te vullen", {
    skip_on_cran()
  })


})
