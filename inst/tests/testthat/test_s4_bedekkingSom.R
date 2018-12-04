context("test s4_BedekkingSom")

library(methods)

describe("s4_BedekkingSom", {

  it("Berekening BerekenWaarde gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingSom",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1", "Z"),
              TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
              WaardeMin = c(rep(1, 5), 0.5),
              WaardeMax = 1,
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1,
              SubTaxonId = 2,
              stringsAsFactors = FALSE
            ),
          Studiegroep =
            data.frame(
              Waarde = "Z"
            )
        )
      ),
      c(1.5, 2)
    )
  })
})
