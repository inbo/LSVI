context("test s4_Aantal")

describe("s4_Aantal", {

  it("In- en uitvoer van slots gebeurt correct", {
    expect_identical(
      new(Class = "aantal")@VoorwaardeID,
      numeric(0)
    )
    expect_equal(
      nrow(new(Class = "aantal")@Soortensubgroep),
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
            TypeKenmerk = c("soort_nbn", "studiegroep"),
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
              WaardeMax = 1,
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NBNTaxonVersionKey = c("A1", "B1", "C1", "E1"),
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
              WaardeMax = c(1, 1, 1, 1, 0),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NBNTaxonVersionKey = c("A1", "B1", "C1", "E1"),
              stringsAsFactors = FALSE
            )
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
              Kenmerk = character(0),
              TypeKenmerk = character(0),
              WaardeMax = numeric(0),
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NBNTaxonVersionKey = c("A1", "B1", "C1", "E1"),
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
              Kenmerk = c("A1", "B2", "C1", "D3", "E1"),
              TypeKenmerk = "soort_nbn",
              WaardeMax = 1,
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NBNTaxonVersionKey = character(0),
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
              stringsAsFactors = FALSE
            ),
          Soortengroep =
            data.frame(
              NBNTaxonVersionKey = c("A1", "B1", "C1", "E1"),
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

  #nog toevoegen als het opzoeken van soorten in subniveaus opgelost is: de aggregatie van bedekkingen gebeurt correct

})
