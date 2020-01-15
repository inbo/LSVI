context("complexe combinatie van voorwaarden")

library(dplyr)
maakConnectiePool()
Data_habitat <-
  data.frame(ID = 1, Habitattype = "7150", stringsAsFactors = FALSE)
Data_voorwaarden <-
  data.frame(
    ID = 1,
    Criterium = "Vegetatie",
    Indicator = "sleutelsoorten",
    Voorwaarde = c("aantal sleutelsoorten", "aantal sleutelsoorten frequent",
                   "aanwezigheid specifieke sleutelsoorten"),
    Waarde = 0,
    Type = "Geheel getal",
    Invoertype = NA_character_,
    Eenheid = NA_character_,
    stringsAsFactors = FALSE
  )

describe("4 voorwaarden combineren", {
  it("combinatie vw sleutelsoorten bij 7150 gebeurt correct", {
    expect_equal(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        Data_habitat = Data_habitat,
        Data_voorwaarden = Data_voorwaarden
      )[["Resultaat_indicator"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Status_indicator, Verschilscore),
      data.frame(
        Status_indicator = FALSE,
        Verschilscore = -1,
        stringsAsFactors = FALSE
      )
    )
    expect_equal(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        Data_habitat = Data_habitat,
        Data_voorwaarden = Data_voorwaarden %>%
          mutate(Waarde = Waarde + 1)
      )[["Resultaat_detail"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Voorwaarde, Waarde, Status_voorwaarde, Verschilscore),
      data.frame(
        Voorwaarde =
          c("aantal sleutelsoorten", "aantal sleutelsoorten frequent",
            "aantal sleutelsoorten frequent",
            "aanwezigheid specifieke sleutelsoorten"),
        Waarde = "1",
        Status_voorwaarde = c(FALSE, FALSE, TRUE, TRUE),
        Verschilscore = c(-0.666666666666667, -0.5, NA_real_, NA_real_),
        stringsAsFactors = FALSE
      )
    )
    expect_equal(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        Data_habitat = Data_habitat,
        Data_voorwaarden = Data_voorwaarden %>%
          mutate(Waarde = Waarde + 1)
      )[["Resultaat_indicator"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Status_indicator, Verschilscore) %>%
        data.frame(),
      data.frame(
        Status_indicator = FALSE,
        Verschilscore = NA_real_,
        stringsAsFactors = FALSE
      )
    )
    expect_equal(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        Data_habitat = Data_habitat,
        Data_voorwaarden = Data_voorwaarden %>%
          mutate(Waarde = Waarde + 2)
      )[["Resultaat_indicator"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Status_indicator, Verschilscore) %>%
        data.frame(),
      data.frame(
        Status_indicator = TRUE,
        Verschilscore = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  })
})

library(pool)
poolClose(ConnectiePool)