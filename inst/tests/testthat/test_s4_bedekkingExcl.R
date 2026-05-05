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
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(1, 0, 1, 0, 1),
              WaardeMax = 1,
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
      c(0, 1)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingExcl",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1", "X1", "X2", "Y1"),
              TypeKenmerk = c(rep("soort_nbn", 5), rep("studiegroep", 3)),
              WaardeMin = c(1, 0, 1, 0, 1, 0, 1, 0),
              WaardeMax = 1,
              Eenheid = "%",
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
              TypeKenmerk = "soort_nbn",
              Vegetatielaag = c("X1", "X2", "Y1", "X1", "Y1"),
              WaardeMin = c(1, 1, 1, 0, 1),
              WaardeMax = 1,
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
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
      "aan- of afwezigheid bedekking"
    )
    expect_equal(
      Testresultaat,
      c(NA, NA)
    )
  })
})
