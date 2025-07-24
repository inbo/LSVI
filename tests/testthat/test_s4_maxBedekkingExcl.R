context("test s4_maxBedekkingExcl")

library(methods)

describe("s4_maxBedekkingExcl", {

  it("Berekening BerekenWaarde gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "maxBedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = c(0.5, 0, 0.6, 0.2, 0.3),
              WaardeMax = c(0.7, 0, 0.8, 0.7, 0.3),
              Eenheid = "%",
              Rank = "SPECIES",
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1"),
              Vegetatielaag = "kruidlaag",
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
      c(0.2, 0.7)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "maxBedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C2", "D3", "E2"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = c(0.5, 0, 0.6, 0.2, 0.3),
              WaardeMax = c(0.7, 0, 0.8, 0.7, 0.4),
              Eenheid = "%",
              Rank = "SPECIES",
              SpeciesKey = c("A1", "B2", "C2", "D3", "E2"),
              Vegetatielaag = "kruidlaag",
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
      c(0.6, 0.8)
    )
  })
  it("Aan-/afwezig geeft NA en een warning", {
    expect_warning(
      Testresultaat <- berekenWaarde(
        new(
          Class = "maxBedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = NA,
              Rank = "SPECIES",
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1"),
              Vegetatielaag = "kruidlaag",
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
      )
    )
    expect_equal(
      Testresultaat,
      c(NA, NA)
    )
  })
})
