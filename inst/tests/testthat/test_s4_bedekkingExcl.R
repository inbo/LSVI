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
  })
})
