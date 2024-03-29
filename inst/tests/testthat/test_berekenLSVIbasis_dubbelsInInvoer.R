context("test berekenLSVIbasis: dubbels in invoer")

library(readr)
library(dplyr)
library(rlang)

maakConnectiePool()
Data_habitat <- #nolint
    read_csv2(
      system.file("vbdata/Test9190habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character())
    )
Data_voorwaarden <- #nolint
  read_csv2(
    system.file("vbdata/Test9190voorwaarden.csv", package = "LSVI"),
    col_types =
      list(
        col_character(), col_character(), col_character(), col_character(),
        col_character(), col_character(), col_character(), col_character()
      )
  )
Data_soortenKenmerken <- #nolint
  read_csv2(
    system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
    col_types =
      list(col_character(), col_character(), col_character(),
           col_character(), col_character(), col_character(),
           col_character(), col_character())
  )

describe("Data_voorwaarden", {
  it("dubbele invoer geeft een error", {
    Data_voorwaardenDubbel <- Data_voorwaarden %>% #nolint
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
      " 1 is de voorwaarde 'msa' meermaals opgegeven"
    )
    Data_voorwaardenDubbel <- Data_voorwaarden %>% #nolint
      bind_rows(
        data.frame(
          ID = "1", Criterium = "Structuur",
          Indicator = "minimum structuurareaal",
          Waarde = "TRUE", stringsAsFactors = FALSE
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
      " 1 is de indicator 'minimum structuurareaal' tweemaal opgegeven"
    )
  })
})

describe("Data_soortenKenmerken", {
  it("dubbele invoer geeft een error", {
    Data_soortenKenmerkenDubbel <- Data_soortenKenmerken %>% #nolint
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
      "'Gewone vlier' meermaals opgegeven voor de boomlaag" #nolint
    )
    Data_soortenKenmerkenDubbel <- Data_soortenKenmerken %>% #nolint
      bind_rows(
        data.frame(
          ID = "1", Kenmerk = "Sambucus nigra",
          TypeKenmerk = "soort_Latijn", Waarde = "2", Type = "Percentage",
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
      "zowel Nederlandse als Latijnse namen gebruikt voor de soort 'Sambucus nigra'" #nolint
    )
  })
})

library(pool)
poolClose(ConnectiePool)
