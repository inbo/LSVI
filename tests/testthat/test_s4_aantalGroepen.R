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
              TypeKenmerk = "soort_gbif",
              WaardeMin = c(0.5, 0, 0.6, 0, 0),
              WaardeMax = c(0.5, 0.3, 0.6, 0.3, 1),
              Eenheid = "%",
              Rank = c(rep("SPECIES", 3), "SUBSPECIES", "SPECIES"),
              SpeciesKey = c("A1", "B2", "C1", "D", "E1"),
              SubspeciesKey = c(rep(NA, 3), "D3", NA),
              Vegetatielaag = "kruidlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonGroepCode = c(rep("1", 3), "2"),
              Rank = "SPECIES",
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
              TypeKenmerk = "soort_gbif",
              WaardeMin = c(0.5, 0, 0.6, 0, 0),
              WaardeMax = c(0.5, 0.3, 0.6, 0.3, 0),
              Eenheid = "%",
              Rank = c(rep("SPECIES", 3), "SUBSPECIES", "SPECIES"),
              SpeciesKey = c("A1", "B2", "C1", "D", "E1"),
              SubspeciesKey = c(rep(NA, 3), "D3", NA),
              Vegetatielaag = "kruidlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonGroepCode = c(rep("1", 3), "2"),
              Rank = "SPECIES",
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
              TypeKenmerk = "soort_gbif",
              WaardeMin = NA,
              WaardeMax = c(rep(0, 4), 1),
              Eenheid = "ja/nee",
              Rank = c(rep("SPECIES", 3), "SUBSPECIES", "SPECIES"),
              SpeciesKey = c("A1", "B2", "C1", "D", "E1"),
              SubspeciesKey = c(rep(NA, 3), "D3", NA),
              Vegetatielaag = "kruidlaag",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonGroepCode = c(rep("1", 3), "2"),
              Rank = "SPECIES",
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
              TypeKenmerk = c(rep("soort_gbif", 5), "studiegroep"),
              WaardeMin = c(0.5, 0, 0, 0, 0, 0.8),
              WaardeMax = c(0.5, 0.3, 0, 0.3, 1, 0.8),
              Eenheid = "%",
              Rank = c(rep("SPECIES", 3), "SUBSPECIES", "SPECIES", NA),
              SpeciesKey = c("A1", "B2", "C1", "D", "E1", NA),
              SubspeciesKey = c(rep(NA, 3), "D3", rep(NA, 2)),
              Vegetatielaag = c(rep("kruidlaag", 5), NA),
              LijstNaam = c(rep(NA, 5), "groeivormen"),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              GbifUsageKey = c("A1", "B1", "C1", "E1"),
              TaxonGroepCode = c(rep("1", 3), "2"),
              Rank = "SPECIES",
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
