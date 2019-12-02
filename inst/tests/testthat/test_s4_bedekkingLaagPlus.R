context("test s4_BedekkingLaagPlus")

library(methods)
library(dplyr)

describe("s4_bedekkingLaagPlus", {

  it("Berekening bedekking vegetatielaag + soorten gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingLaagPlus",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = c(0.5, 0, 0.6, 0, 0),
              WaardeMax = c(0.5, 0.3, 0.6, 0.3, 1),
              Eenheid = "%",
              Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", "2")),
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
              Waarde = "moslaag",
              LijstNaam = "Vegetatielaag",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0.8, 1)
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "bedekkingLaagPlus",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1", "moslaag"),
              TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
              WaardeMin = c(0.5, 0, 0, 0, 0, 0.8),
              WaardeMax = c(0.5, 0.3, 0, 0.3, 1, 0.8),
              Eenheid = "%",
              Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", "2"), NA),
              LijstNaam = c(rep(NA, 5), "Vegetatielaag"),
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
              Waarde = "moslaag",
              LijstNaam = "Vegetatielaag",
              stringsAsFactors = FALSE
            )
        )
      ),
      c(0.8, 1)
    )
  })
})
