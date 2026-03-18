context("test s4_aandeelKruidlaag")

library(methods)

describe("s4_aandeelKruidlaag", {

  it("Berekening BerekenWaarde gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeelKruidlaag",
          Kenmerken = data.frame(
            Kenmerk = c("A1", "B2", "C1", "D3", "E1", "kruidlaag"),
            TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
            WaardeMin = c(rep(0.1, 5), 0.5),
            WaardeMax = c(rep(0.3, 5), 0.6),
            Eenheid = "%",
            Vegetatielaag = c(rep("kruidlaag", 5), NA),
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde = "kruidlaag",
            LijstNaam = "kruidlaag",
            stringsAsFactors = FALSE
          )
        )
      ),
      c(
        (1.0 - prod(1.0 - rep(0.1, 3))) / 0.6,
        (1.0 - prod(1.0 - rep(0.3, 3))) / 0.5
      )
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeelKruidlaag",
          Kenmerken = data.frame(
            Kenmerk = c("A1", "B2", "C1", "D3", "E1", "kruidlaag", "moslaag"),
            TypeKenmerk = c(rep("soort_nbn", 5), rep("studiegroep", 2)),
            WaardeMin = c(rep(0.1, 5), 0.5, 0.1),
            WaardeMax = c(rep(0.3, 5), 0.6, 0.15),
            Eenheid = "%",
            Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", 2), NA, NA),
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde = c("moslaag", "kruidlaag", "kruid- en moslaag"),
            LijstNaam = "kruid- en moslaag",
            stringsAsFactors = FALSE
          )
        )
      ),
      c(
        (1.0 - prod(1.0 - rep(0.1, 3))) / (1.0 - prod(1.0 - c(0.6, 0.15))),
        (1.0 - prod(1.0 - rep(0.3, 3))) / (1.0 - prod(1.0 - c(0.5, 0.1)))
      )
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeelKruidlaag",
          Kenmerken = data.frame(
            Kenmerk =
              c("A1", "B2", "C1", "D3", "E1", "kruidlaag", "naakte bodem"),
            TypeKenmerk = c(rep("soort_nbn", 5), rep("studiegroep", 2)),
            WaardeMin = c(rep(0.1, 5), 0.5, 0.3),
            WaardeMax = c(rep(0.3, 5), 0.6, 0.4),
            Eenheid = "%",
            Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", 2), NA, NA),
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde = c(
              "totale vegetatiebedekking", "naakte bodem", "kruidlaag",
              "moslaag"
            ),
            LijstNaam = "totale vegetatiebedekking",
            stringsAsFactors = FALSE
          )
        )
      ),
      c(
        (1.0 - prod(1.0 - rep(0.1, 3))) / (1 - 0.3),
        (1.0 - prod(1.0 - rep(0.3, 3))) / (1 - 0.4)
      )
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aandeelKruidlaag",
          Kenmerken = data.frame(
            Kenmerk =
              c("A1", "B2", "C1", "D3", "E1", "totale vegetatiebedekking"),
            TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
            WaardeMin = c(rep(0.1, 5), 0.5),
            WaardeMax = c(rep(0.3, 5), 0.6),
            Eenheid = "%",
            Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", 2), NA),
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde = c(
              "totale vegetatiebedekking", "naakte bodem", "moslaag",
              "kruidlaag"
            ),
            LijstNaam = "totale vegetatiebedekking",
            stringsAsFactors = FALSE
          )
        )
      ),
      c(
        (1.0 - prod(1.0 - rep(0.1, 3))) / 0.6,
        (1.0 - prod(1.0 - rep(0.3, 3))) / 0.5
      )
    )
  })
  it("Aantal met bedekking in kruidlaag", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken = data.frame(
            Kenmerk = c("A1", "B2", "C1", "D3", "E1", "naakte bodem"),
            TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
            WaardeMin = c(rep(0.2, 5), 0.5),
            WaardeMax = c(rep(0.3, 5), 0.6),
            Eenheid = "%",
            Vegetatielaag = c(rep("kruidlaag", 5), NA),
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde =
              c("totale vegetatiebedekking", "naakte bodem", "kruidlaag"),
            LijstNaam = "totale vegetatiebedekking",
            stringsAsFactors = FALSE
          ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.3,
          SubRefMax = 0.4,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken = data.frame(
            Kenmerk =
              c("A1", "B2", "C1", "D3", "E1", "totale vegetatiebedekking"),
            TypeKenmerk = c(rep("soort_nbn", 5), "studiegroep"),
            WaardeMin = c(rep(0.2, 5), 0.4),
            WaardeMax = c(rep(0.3, 5), 0.5),
            Eenheid = "%",
            Vegetatielaag = c(rep("kruidlaag", 5), NA),
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde =
              c("totale vegetatiebedekking", "naakte bodem", "kruidlaag"),
            LijstNaam = "totale vegetatiebedekking",
            stringsAsFactors = FALSE
          ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.3,
          SubRefMax = 0.4,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_error(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken = data.frame(
            Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
            TypeKenmerk = "soort_nbn",
            WaardeMin = 0.2,
            WaardeMax = 0.3,
            Eenheid = "%",
            Vegetatielaag = "kruidlaag",
            stringsAsFactors = FALSE
          ),
          Soortengroep = data.frame(
            NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
            TaxonId = 1:4,
            SubTaxonId = 1:4,
            stringsAsFactors = FALSE
          ),
          Studiegroep = data.frame(
            Waarde =
              c("totale vegetatiebedekking", "naakte bodem", "kruidlaag"),
            LijstNaam = "totale vegetatiebedekking",
            stringsAsFactors = FALSE
          ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.3,
          SubRefMax = 0.4,
          SubOperator = ">="
        )
      ),
      "Om de bedekking te kunnen berekenen ten opzichte van de totale vegetatiebedekking, is het nodig om studiegroep 'naakte bodem' of 'totale vegetatiebedekking' op te geven." #nolint: line_length_linter
    )
  })
})
