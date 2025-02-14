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
              TypeKenmerk = c(rep("soort_gbif", 5), "studiegroep"),
              WaardeMin = c(rep(1, 5), 0.5),
              WaardeMax = 1,
              Eenheid = "%",
              Rank = c(rep("SPECIES", 5), NA),
              SpeciesKey = c("A1", "B2", "C1", "D3", "E1", NA),
              Vegetatielaag = c(rep("kruidlaag", 5), NA),
              LijstNaam = "lijst",
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
              Waarde = "Z",
              LijstNaam = "lijst"
            )
        )
      ),
      c(1.5, 2)
    )
  })
})
