context("test s4_aantalGroepen")

library(methods)

describe("s4_aantalGroepen", {

  it("Berekening aantal groepen gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantalGroepen",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(0.5, 0, 0.6, 0, 0),
              WaardeMax = c(0.5, 0.3, 0.6, 0.3, 1),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxongroepId = 1,
              TaxonsubgroepId = c(rep(1, 3), 2),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = "helofyten",
              LijstNaam = "groeivormen",
              stringsAsFactors = FALSE
            )
        )
      ),
      2
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantalGroepen",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(0.5, 0, 0.6, 0, 0),
              WaardeMax = c(0.5, 0.3, 0.6, 0.3, 0),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxongroepId = 1,
              TaxonsubgroepId = c(rep(1, 3), 2),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = "helofyten",
              LijstNaam = "groeivormen",
              stringsAsFactors = FALSE
            )
        )
      ),
      1
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantalGroepen",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = NA,
              WaardeMax = c(rep(0, 4), 1),
              Eenheid = "ja/nee",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxongroepId = 1,
              TaxonsubgroepId = c(rep(1, 3), 2),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = "helofyten",
              LijstNaam = "groeivormen",
              stringsAsFactors = FALSE
            )
        )
      ),
      1
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantalGroepen",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1", "helofyten"),
              TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
              WaardeMin = c(0.5, 0, 0, 0, 0, 0.8),
              WaardeMax = c(0.5, 0.3, 0, 0.3, 1, 0.8),
              Eenheid = "%",
              LijstNaam = c(rep(NA, 5), "groeivormen"),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxongroepId = 1,
              TaxonsubgroepId = c(rep(1, 3), 2),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = c("helofyten", "wortelend"),
              LijstNaam = "groeivormen",
              stringsAsFactors = FALSE
            )
        )
      ),
      1
    )
  })
})
