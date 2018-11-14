context("test berekeningen op basis van bedekking en grondvlak")

library(readr)
library(dplyr)
library(rlang)

Data_habitat <-
    read_csv2(
      system.file("vbdata/data_habitat9130.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(system.file("vbdata/data_voorwaarden9130.csv", package = "LSVI"),
              col_types = list(col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_character(), col_logical(), col_character(), col_character(), col_character()))

Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/datasoortenKenmerken9130.csv", package = "LSVI")
    )

describe("nakijken of er onderscheid gemaakt worden tussen bedekking en grondvlak bij boomsoorten", {
  it("Correcte berekening invasieve exoten boom- en struiklaag op basis van bedekking en sleutelsoorten boomlaag op basis van grondvlak", {
    skip_if_not(
      class(ConnectiePool$.__enclos_env__$private$createObject())[1] ==
        "Microsoft SQL Server",
      "SQL Server niet beschikbaar"
    )
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()

    ResultaatLSVI <- berekenLSVIbasis(
          ConnectieLSVIhabitats = ConnectieLSVIhabitats,
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )

    Resultaat_invEx <- ResultaatLSVI[["Resultaat_detail"]] %>%
      select(Indicator, Voorwaarde, Waarde, Status_voorwaarde) %>%
      filter(Indicator == "invasieve exoten van de boom- en struiklaag")

    Resultaat_ss_boomlaag <- ResultaatLSVI[["Resultaat_detail"]] %>%
      select(Indicator, Voorwaarde, Waarde, Status_voorwaarde) %>%
      filter(Indicator == "sleutelsoorten van de boom- en struiklaag")

    expect_equal(
      round(as.numeric(Resultaat_invEx$Waarde), 2),
      18.95
    )

    expect_equal(
      round(as.numeric(Resultaat_ss_boomlaag$Waarde), 2),
      60.2
    )

  })
})
