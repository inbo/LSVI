context("test s4_Aandeel")

library(methods)

describe("s4_Aandeel", {

  it("Berekening BerekenWaarde gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeel",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = c(0, 1, 0, 1, 1),
              WaardeMax = c(1, 3, 1, 3, 2),
              Eenheid = "Grondvlak_ha",
              Rank = "SPECIES",
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1"),
              Vegetatielaag = "boomlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              Rank = "SPECIES",
              TaxonGroepCode = "1",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0.1, 1)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeel",
          Kenmerken =
            data.frame(
              Kenmerk = c("B2", "D3"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = 0.1,
              WaardeMax = 0.1,
              Eenheid = "grondvlak_ha",
              Rank = "SPECIES",
              SpeciesKey = c("B2", "D3"),
              Vegetatielaag = "boomlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonGroepCode = "1",
              Rank = "SPECIES",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0, 0)
    )
  })
  it("Geen bomen in plot geeft resultaat 0", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeel",
          Kenmerken =
            data.frame(
              Kenmerk = c("B2", "D3"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = 0,
              WaardeMax = 0.1,
              Eenheid = "%",
              Rank = "SPECIES",
              SpeciesKey = c("B2", "D3"),
              Vegetatielaag = "boomlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonGroepCode = "1",
              Rank = "SPECIES",
              stringsAsFactors = FALSE
            )
        )
      ),
      0
    )
  })
})
