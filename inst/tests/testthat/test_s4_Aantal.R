context("test s4_Aantal")

library(methods)

describe("s4_Aantal", {

  it("In- en uitvoer van slots gebeurt correct", {
    expect_identical(
      new(Class = "aantal")@VoorwaardeID,
      numeric(0)
    )
    expect_equal(
      nrow(new(Class = "aantal")@Soortengroep),
      0
    )
    expect_identical(
      new(Class = "aantal")@SubAnalyseVariabele,
      character(0)
    )
    expect_equal(
      new(Class = "aantal", VoorwaardeID = 123)@VoorwaardeID,
      123
    )
    expect_equal(
      getVoorwaardeID(new(Class = "aantal", VoorwaardeID = 321)),
      321
    )
    expect_error(
      new(Class = "aantal", SubAnalyseVariabele = 122)
    )
    expect_equal(
      getKenmerken(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = c("nbn123", "testgroep"),
              TypeKenmerk = c("soort_nbn", "studiegroep"),
              stringsAsFactors = FALSE
            )
        )
      )$Kenmerk,
      c("nbn123", "testgroep")
    )
    expect_error(
      new(
        Class = "aantal",
        Kenmerken =
          data.frame(
            Kenmerk = c("nbn123", "testgroep"),
            TypeKenmerk = c("soort_nbn", "soortengroep"),
            stringsAsFactors = FALSE
          )
      )
    )
    expect_error(
      new(
        Class = "aantal",
        Kenmerken =
          data.frame(
            Kenmerk = c("nbn123", 3.14),
            TypeKenmerk = c("soort_nl", "studiegroep"),
            stringsAsFactors = FALSE
          )
      )
    )
    expect_error(
      new(
        Class = "aantal",
        Kenmerken =
          data.frame(
            Kenmerk = c("nbn123", "testgroep"),
            Groep = c("soort_nbn", "studiegroep"),
            stringsAsFactors = FALSE
          )
      )
    )
  })

  it("Berekening BerekenWaarde gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = NA,
              WaardeMax = 1,
              Eenheid = "ja/nee",
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
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = NA,
              WaardeMax = c(1, 1, 1, 1, 0),
              Eenheid = "ja/nee",
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
      2
    )
    expect_warning(
      Testresultaat <- berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = character(0),
              TypeKenmerk = character(0),
              WaardeMin = numeric(0),
              WaardeMax = numeric(0),
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
      "geen enkele soort opgegeven"
    )
    expect_equal(
      Testresultaat,
      NA
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = 1,
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = character(0),
              TaxonId = numeric(0),
              SubTaxonId = numeric(0),
              stringsAsFactors = FALSE
            )
        )
      ),
      0
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4, 0.2),
              WaardeMin = c(1, 0.8, 0.6, 0.4, 0.2),
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
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
  })

  it("Berekening BerekenWaarde gebeurt correct voor aan-afwezig", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = "ja/nee",
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
      3
    )
    expect_warning(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              ID = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = "ja/nee",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1", "B2"),
              TaxonId = c(1:4, 4),
              SubTaxonId = 1:5,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      "aan- of afwezigheid aantal"
    )
  })

  it("De aggregatie van bedekkingen bij subsoorten gebeurt correct", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:4,
              Kenmerk = c("A1", "B2", "C1", "D1"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.6),
              WaardeMin = c(1, 0.8, 0.6, 0.6),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:4,
              Kenmerk = c("A1", "B2", "C1", "D3"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4),
              WaardeMin = c(1, 0.8, 0.6, 0.4),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:4,
              Kenmerk = c("A1", "B2", "C1", "D3"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.6),
              WaardeMin = c(1, 0.8, 0.6, 0.6),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "D4"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4, 0.4),
              WaardeMin = c(1, 0.8, 0.6, 0.4, 0.4),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D1", "D3"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4, 0.4),
              WaardeMin = c(1, 0.8, 0.6, 0.4, 0.4),
              Eenheid = "%",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
  })

  it(
    "De aggregatie van bedekkingen/grondvlakaandelen bij subsoorten gebeurt correct" #nolint
    , {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D1", "A1"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.6, 0.5),
              WaardeMin = c(1, 0.8, 0.6, 0.6, 0.5),
              Eenheid = c(rep("%", 4), "grondvlak_ha"),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:4,
              Kenmerk = c("A1", "B2", "C1", "D3"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4),
              WaardeMin = c(1, 0.8, 0.6, 0.4),
              Eenheid = "grondvlak_ha",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "aandeel",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:4,
              Kenmerk = c("A1", "B2", "C1", "D3"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.6),
              WaardeMin = c(1, 0.8, 0.6, 0.6),
              Eenheid = "Grondvlak_ha",
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "aandeel",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      3
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "D4"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4, 0.4),
              WaardeMin = c(1, 0.8, 0.6, 0.4, 0.4),
              Eenheid = c(rep("grondvlak_ha", 4), "%"),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "D1", "D3", "D4"),
              TaxonId = c(1:4, 4, 4),
              SubTaxonId = 1:6,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "aandeel",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
  })

  it("Berekening gebeurt correct als een soort meermaals ingevoerd wordt", {
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "C1"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4, 0.5),
              WaardeMin = c(1, 0.8, 0.4, 0.4, 0.3),
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
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Rijnr = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "C1"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = c(1, 0.8, 0.6, 0.4, 0.5),
              WaardeMin = c(1, 0.8, 0.4, 0.4, 0.3),
              Eenheid = "%",
              Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", "2")),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      2
    )
    expect_equal(
      berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              Kenmerk = c("A1", "B2", "C1", "D3", "C1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = "ja/nee",
              Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", "2")),
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
      2
    )
    expect_warning(
      Testresultaat <- berekenWaarde(
        new(
          Class = "aantal",
          Kenmerken =
            data.frame(
              ID = 1:5,
              Kenmerk = c("A1", "B2", "C1", "D3", "C1"),
              TypeKenmerk = "soort_nbn",
              WaardeMin = 1,
              WaardeMax = NA,
              Eenheid = "ja/nee",
              Vegetatielaag = c(rep("moslaag", 3), rep("kruidlaag", "2")),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NbnTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              TaxonId = 1:4,
              SubTaxonId = 1:4,
              stringsAsFactors = FALSE
            ),
          SubAnalyseVariabele = "bedekking",
          SubRefMin = 0.5,
          SubRefMax = 0.5,
          SubOperator = ">="
        )
      ),
      "aan- of afwezigheid aantal"
    )
    expect_equal(
      Testresultaat,
      c(0, 2)
    )
  })
})
