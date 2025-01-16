context("test s4_BedekkingExcl")

library(methods)

describe("s4_BedekkingExcl", {

  it("Berekening BerekenWaarde gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_gbif",
              WaardeMin = c(1, 0, 1, 0, 1),
              WaardeMax = 1,
              Eenheid = "%",
              Rank = "SPECIES",
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1"),
              Vegetatielaag = "kruidlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              Rank = "SPECIES",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0, 1)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1", "X1", "X2", "Y1"),
              TypeKenmerk = c(rep("soort_gbif", 5), rep("studiegroep", 3)),
              WaardeMin = c(1, 0, 1, 0, 1, 0, 1, 0),
              WaardeMax = 1,
              Eenheid = "%",
              Rank = c(rep("SPECIES", 5), rep(NA, 3)),
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1", rep(NA, 3)),
              Vegetatielaag = c(rep("kruidlaag", 5), rep(NA, 3)),
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = c("X1", "Y1", "Z1"),
              Volgnummer = 1:3,
              LijstNaam = "laag",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(1, 1)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_gbif",
              Vegetatielaag = c("X1", "X2", "Y1", "X1", "Y1"),
              WaardeMin = c(1, 1, 1, 0, 1),
              WaardeMax = 1,
              Eenheid = "%",
              Rank = "SPECIES",
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1"),
              Vegetatielaag = "kruidlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              Rank = "SPECIES",
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = c("X1", "Y1", "Z1"),
              Volgnummer = 1:3,
              LijstNaam = "laag",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0, 1)
    )
  })
  it("Aan-/afwezig geeft NA en een warning", {
    expect_warning(
      Testresultaat <- berekenWaarde(
        new(
          Class = "bedekkingExcl",
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
              TaxonId = 1:4,
              Rank = "SPECIES",
              stringsAsFactors = FALSE
            )
        )
      ),
      "aan- of afwezigheid bedekking"
    )
    expect_equal(
      Testresultaat,
      c(NA, NA)
    )
  })
})
