context("test databank")

library(DBI)
library(dplyr)

describe("test databank", {
  it("Lijstitems hebben een ondergrens en bovengrens", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    expect_true(
      all(
        !is.na(
          dbGetQuery(ConnectieLSVIhabitats, "SELECT Ondergrens FROM LijstItem")
        )
      )
    )
    expect_true(
      all(
        !is.na(
          dbGetQuery(ConnectieLSVIhabitats, "SELECT Bovengrens FROM LijstItem")
        )
      )
    )
  })

  it(
    "In de databank zitten enkel AnalyseVariabelen waarvoor code ontwikkeld is"
    , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT DISTINCT(VariabeleNaam) FROM AnalyseVariabele"
      ) %>%
      filter(!grepl("^meting", .data$VariabeleNaam))
    expect_true(
      all(AV$VariabeleNaam %in%
            c("aandeel", "aandeelExcl", "aandeelKruidlaag", "aandeelSom",
              "aantal", "bedekking", "bedekkingLaag", "maxBedekking",
              "maxBedekkingExcl")
      )
    )
  })

  it("AnalyseVariabele aantal bevat enkel gehele getallen", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
          TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aantal'"
      )
    skip_if_not(nrow(AV) > 0, "aantal komt niet voor")
    AV_ok <- AV %>%
      filter(TypeVariabele == "Geheel getal")
    AV_leeg <- AV %>%
      filter(TypeVariabele != "Geheel getal")
    expect_equal(
      nrow(AV_ok),
      1
    )
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          AV_ok$Id
        )
      )
    expect_true(
      all(
        as.numeric(Refwaarden$Referentiewaarde) -
          round(as.numeric(Refwaarden$Referentiewaarde)) == 0
      )
    )
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
  })

  it("AnalyseVariabele bedekking bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
          TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekking'"
      )
    skip_if_not(nrow(AV) > 0, "bedekking komt niet voor")
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage", "Categorie"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT VoorwaardeNaam, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Percentage"))$Id
        )
      )
    RefwaardenPerc <- RefwaardenPerc %>%
      filter(!.data$Referentiewaarde %in% RefwaardenPerc$VoorwaardeNaam)
    expect_true(
      all(as.numeric(RefwaardenPerc$Referentiewaarde) <= 100)
    )
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    skip_if_not(nrow(AV_ok) == 2, "Geen categorische var voor bedekking")
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Categorie"))$Id
        )
      )
    LijstItems <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Waarde FROM LijstItem"
      )
    expect_true(
      all(RefwaardenCat$Referentiewaarde %in% LijstItems$Waarde)
    )
  })

  it("AnalyseVariabele aandeel bevat percentages", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeel'"
      )
    skip_if_not(nrow(AV) > 0, "aandeel komt niet voor")
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Percentage"))$Id
        )
      )
    expect_true(
      all(as.numeric(RefwaardenPerc$Referentiewaarde) <= 100)
    )
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
  })

  it("AnalyseVariabele aandeelExcl bevat percentages", {
    skip_on_cran()
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeelExcl'"
      )
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
  })

  it("AnalyseVariabele aandeelKruidlaag bevat percentages", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeelKruidlaag'"
      )
    skip_if_not(nrow(AV) > 0, "aandeelKruidlaag komt niet voor")
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Percentage"))$Id
        )
      )
    expect_true(
      all(as.numeric(RefwaardenPerc$Referentiewaarde) <= 100)
    )
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
  })

  it("AnalyseVariabele aandeelSom bevat percentages", {
    skip_on_cran()
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeelSom'"
      )
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
  })

  it("AnalyseVariabele maxBedekking bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekking'"
      )
    skip_if_not(nrow(AV) > 0, "maxBedekking komt niet voor")
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage", "Categorie"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Percentage"))$Id
        )
      )
    expect_true(
      all(as.numeric(RefwaardenPerc$Referentiewaarde) <= 100)
    )
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    skip_if_not(nrow(AV_ok) == 2, "Geen categorische var voor maxBedekking")
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Categorie"))$Id
        )
      )
    LijstItems <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Waarde FROM LijstItem"
      )
    expect_true(
      all(RefwaardenCat$Referentiewaarde %in% LijstItems$Waarde)
    )
  })

  it("AnalyseVariabele maxBedekkingExcl bevat percentages en categorische var"
     , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekkingExcl'"
      )
    skip_if_not(nrow(AV) > 0, "maxBedekkingExcl komt niet voor")
    AV_ok <- AV %>%
      filter(TypeVariabele %in% c("Percentage", "Categorie"))
    AV_leeg <- AV %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Percentage"))$Id
        )
      )
    expect_true(
      all(as.numeric(RefwaardenPerc$Referentiewaarde) <= 100)
    )
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    skip_if_not(nrow(AV_ok) == 2, "Geen categorische var voor maxBedekkingExcl")
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (AV %>% filter(TypeVariabele == "Categorie"))$Id
        )
      )
    LijstItems <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Waarde FROM LijstItem"
      )
    expect_true(
      all(RefwaardenCat$Referentiewaarde %in% LijstItems$Waarde)
    )
  })

  it(
    "AnalyseVariabele aantal heeft telkens een SoortengroepId of StudiegroepId"
    , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aantal'"
      )
    skip_if_not(nrow(AV) > 0, "aantal komt niet voor")
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT TaxongroepId, StudiegroepId FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(AV$Id, collapse = "','")
        )
      )
    expect_true(
      all(!is.na(Refwaarden$TaxongroepId) | !is.na(Refwaarden$StudiegroepId))
    )
  })

  it(
    "AnalyseVariabele bedekking heeft telkens een SoortengroepId of StudiegroepId" #nolint
    , {
      ConnectieLSVIhabitats <-
        connecteerMetLSVIlite()
      AV <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekking'"
        )
      skip_if_not(nrow(AV) > 0, "bedekking komt niet voor")
      Refwaarden <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          sprintf(
            "SELECT TaxongroepId, StudiegroepId FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
            paste(AV$Id, collapse = "','")
          )
        )
      expect_true(
        all(!is.na(Refwaarden$TaxongroepId) | !is.na(Refwaarden$StudiegroepId))
      )
    })

  it("TypeVariabele Vrije tekst is nergens gebruikt", {
      ConnectieLSVIhabitats <-
        connecteerMetLSVIlite()
      AV <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          "SELECT Voorwaarde.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM Voorwaarde INNER JOIN AnalyseVariabele
          ON Voorwaarde.AnalyseVariabeleId = AnalyseVariabele.Id
        INNER JOIN TypeVariabele
          ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE TypeVariabele.Naam = 'Vrije tekst'"
        )
      expect_equal(
        nrow(AV),
        0
      )
    })

  it("Voor elke categorische variabele is een Invoermasker opgegeven", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Voorwaarde.Id, Voorwaarde.InvoermaskerId,
        AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM Voorwaarde INNER JOIN AnalyseVariabele
          ON Voorwaarde.AnalyseVariabeleId = AnalyseVariabele.Id
        INNER JOIN TypeVariabele
          ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE TypeVariabele.Naam = 'Categorie'"
      )
    expect_true(
      all(!is.na(AV$InvoermaskerId))
    )
  })

  it("De subanalysevariabele is overal correct ingevoerd (enkel bedekking)", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Voorwaarde.Id, Voorwaarde.SubInvoermaskerId,
        AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM Voorwaarde INNER JOIN AnalyseVariabele
        ON Voorwaarde.SubAnalyseVariabeleId = AnalyseVariabele.Id
        INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE TypeVariabele.Naam = 'Categorie'"
      )
    expect_true(
      all(AV$VariabeleNaam == "bedekking")
    )
    expect_true(
      all(AV$TypeVariabele %in% c("Categorie", "Percentage"))
    )
    AV_cat <- AV %>%
      filter(TypeVariabele == "Categorie")
    expect_true(
      all(!is.na(AV_cat$SubInvoermaskerId))
    )
  })

})
