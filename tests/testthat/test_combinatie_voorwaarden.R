context("complexe combinatie van voorwaarden")

library(dplyr)
maakConnectiePool()
data_habitat <-
  data.frame(ID = 1, Habitattype = "7150", stringsAsFactors = FALSE)
data_voorwaarden <-
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

WarningHorizStructuur <-
  "De waarde\\(n\\) voor de voorwaarde\\(n\\) oppervlakte moeras in ha, oppervlakte habitatvlek in ha \\(VoorwaardeID 2516, 2500\\) kunnen niet berekend worden voor opname\\(n\\) 1. Geef de waarde voor deze voorwaarde rechtstreeks in als input van de functie 'berekenLSVIBasis' via tabel 'Data_voorwaarden' \\(zie \\?berekenLSVIbasis voor meer info\\). Vermeld hierbij Criterium = Structuur, Indicator = horizontale structuur en Voorwaarde = oppervlakte moeras in ha, oppervlakte habitatvlek in ha." #nolint: line_length_linter

describe("4 voorwaarden combineren", {
  it("combinatie vw sleutelsoorten bij 7150 gebeurt correct", {
    expect_warning(
      TestResultaat <- berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        data_habitat,
        data_voorwaarden
      ),
      WarningHorizStructuur
    )
    expect_equal(
      TestResultaat[["Resultaat_indicator"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Status_indicator, Verschilscore),
      tibble(
        Status_indicator = FALSE,
        Verschilscore = -1
      )
    )
    expect_warning(
      TestResultaat <- berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        data_habitat,
        data_voorwaarden %>%
          mutate(Waarde = Waarde + 1)
      ),
      WarningHorizStructuur
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Voorwaarde, Waarde, Status_voorwaarde, Verschilscore),
      data.frame(
        Voorwaarde =
          c("aantal sleutelsoorten", "aantal sleutelsoorten frequent",
            "aantal sleutelsoorten frequent",
            "aanwezigheid specifieke sleutelsoorten"),
        Waarde = "1",
        Status_voorwaarde = c(FALSE, FALSE, TRUE, TRUE),
        Verschilscore = c(-0.666666666666667, -0.5, 0, 0),
        stringsAsFactors = FALSE
      )
    )
    expect_equal(
      TestResultaat[["Resultaat_indicator"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Status_indicator, Verschilscore) %>%
        data.frame(),
      data.frame(
        Status_indicator = FALSE,
        Verschilscore = -0.5,
        stringsAsFactors = FALSE
      )
    )
    expect_warning(
      TestResultaat <- berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = 1,
        data_habitat,
        data_voorwaarden %>%
          mutate(Waarde = Waarde + 2)
      ),
      WarningHorizStructuur
    )
    expect_equal(
      TestResultaat[["Resultaat_indicator"]] %>%
        filter(Indicator == "sleutelsoorten") %>%
        select(Status_indicator, Verschilscore) %>%
        data.frame(),
      data.frame(
        Status_indicator = TRUE,
        Verschilscore = 0,
        stringsAsFactors = FALSE
      )
    )
  })
})

library(pool)
poolClose(ConnectiePool)
