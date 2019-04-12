context("test berekenLSVIbasis: dubbels in invoer")

library(readr)
library(dplyr)
library(rlang)

maakConnectiepoolSQLite()
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
  )
Data_soortenKenmerken <-
  read_csv2(
    system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
    col_types =
      list(col_character(), col_character(), col_character(),
           col_character(), col_character(), col_character(),
           col_character(), col_character())
  )

describe("Data_voorwaarden", {
  it("dubbele invoer geeft een error", {
    Data_voorwaardenDubbel <- Data_voorwaarden %>%
      bind_rows(
        data.frame(
          ID = "1", Criterium = "Structuur",
          Indicator = "minimum structuurareaal", Voorwaarde = "MSA",
          Waarde = "60", Type = "Decimaal getal", stringsAsFactors = FALSE
        )
      )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaardenDubbel,
        Data_soortenKenmerken
      ),
      "Voor opname(n) 1 is de voorwaarde 'MSA' meermaals opgegeven."
    )
  })
})

describe("Data_soortenKenmerken", {
  it("dubbele invoer geeft een warning en resultaten worden samengenomen", {
    Data_soortenKenmerkenDubbel <- Data_soortenKenmerken %>%
      bind_rows(
        data.frame(
          ID = "1", Kenmerk = "Gewone vlier",
          TypeKenmerk = "soort_NL", Waarde = "2", Type = "Percentage",
          Eenheid = "%", Vegetatielaag = "boomlaag", stringsAsFactors = FALSE
        )
      )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerkenDubbel
      ),
      "Voor opname(n) 1 is de soort 'Gewone vlier' meermaals opgegeven voor dezelfde vegetatielaag en meeteenheid." #nolint
    )
  })
})

library(pool)
poolClose(ConnectiePool)
