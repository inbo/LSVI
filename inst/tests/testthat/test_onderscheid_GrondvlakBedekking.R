context("test berekeningen op basis van bedekking en grondvlak")

library(readr)
library(dplyr)
library(rlang)
maakConnectiePool()

Data_habitat <- #nolint
    read_csv2(
      system.file("vbdata/data_habitat9130.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <- #nolint
    read_csv2(
      system.file("vbdata/data_voorwaarden9130.csv", package = "LSVI"),
      col_types =
        list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_logical(),
          col_character(), col_character(), col_character()
        )
    )

Data_soortenKenmerken <- #nolint
    read_csv2(
      system.file("vbdata/datasoortenKenmerken9130.csv", package = "LSVI")
    )

describe("nakijken of er onderscheid gemaakt worden tussen bedekking en grondvlak bij boomsoorten", {#nolint
  it("Correcte berekening invasieve exoten boom- en struiklaag op basis van bedekking en sleutelsoorten boomlaag op basis van grondvlak", {#nolint
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()

    expect_warning(
      ResultaatLSVI <- berekenLSVIbasis(
          ConnectieLSVIhabitats = ConnectieLSVIhabitats,
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        ),
      "De waarde\\(n\\) voor de voorwaarde\\(n\\) schaalgrootte ingrepen \\(ha\\) \\(VoorwaardeID 2550\\) kunnen niet berekend worden voor opname\\(n\\) 4945. Geef de waarde voor deze voorwaarde rechtstreeks in als input van de functie 'berekenLSVIBasis' via tabel 'Data_voorwaarden' \\(zie \\?berekenLSVIbasis voor meer info\\). Vermeld hierbij Criterium = Structuur, Indicator = horizontale structuur - natuurlijke mozaiekstructuur en Voorwaarde = schaalgrootte ingrepen \\(ha\\)."
    )
    

    resultaat_inv_ex <- ResultaatLSVI[["Resultaat_detail"]] %>%
      select(Indicator, Voorwaarde, Waarde, Status_voorwaarde) %>%
      filter(Indicator == "invasieve exoten van de boom- en struiklaag")

    resultaat_ss_boomlaag <- ResultaatLSVI[["Resultaat_detail"]] %>%
      select(Indicator, Voorwaarde, Waarde, Status_voorwaarde) %>%
      filter(Indicator == "sleutelsoorten van de boom- en struiklaag")

    expect_equal(
      round(as.numeric(resultaat_inv_ex$Waarde), 2),
      18.95
    )

    expect_equal(
      round(as.numeric(resultaat_ss_boomlaag$Waarde), 2),
      60.2
    )

  })
})

library(pool)
poolClose(ConnectiePool)
