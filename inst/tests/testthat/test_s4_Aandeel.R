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
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(0, 1, 0, 1, 1),
              WaardeMax = c(1, 3, 1, 3, 2),
              Eenheid = "Grondvlak_ha",
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
      c(0.1, 1)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeel",
          Kenmerken =
            data.frame(
              Kenmerk = c("B2", "D3"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 0.1,
              WaardeMax = 0.1,
              Eenheid = "Grondvlak_ha",
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
                TypeKenmerk = "soort_nbn",
                WaardeMin = 0,
                WaardeMax = 0.1,
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
        0
      )
  })
})
