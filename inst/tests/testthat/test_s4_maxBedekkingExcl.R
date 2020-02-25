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
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(0.5, 0, 0.6, 0.2, 0.3),
              WaardeMax = c(0.7, 0, 0.8, 0.7, 0.3),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
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
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(0.5, 0, 0.6, 0.2, 0.3),
              WaardeMax = c(0.7, 0, 0.8, 0.7, 0.4),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0.6, 0.8)
    )
  })
  it("Aan-/afwezig geeft NA en een warning", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "maxBedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = NA,
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            )
        )
      ),
      c(NA, NA)
    )
    expect_warning(
      berekenWaarde(
        new(
          Class = "maxBedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = NA,
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            )
        )
      )
    )
  })
})
