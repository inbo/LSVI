context("test databank")

library(DBI)
library(dplyr)
library(stringr)

describe("test databank", {
  it("Lijstitems hebben een ondergrens en bovengrens", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
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
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it(
    "In de databank zitten enkel AnalyseVariabelen waarvoor code ontwikkeld is"
    , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT DISTINCT(VariabeleNaam) FROM AnalyseVariabele"
      ) %>%
      filter(!grepl("^meting", .data$VariabeleNaam))
    expect_true(
      all(av$VariabeleNaam %in%
            c("aandeel", "bedekkingExcl", "aandeelKruidlaag", "bedekkingSom",
              "aantal", "bedekking", "bedekkingLaag", "bedekkingLaagExcl",
              "bedekkingLaagPlus", "maxBedekking", "maxBedekkingExcl",
              "maxBedekking2s", "aantalGroepen", "scoresom")
      )
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele aantal bevat enkel gehele getallen", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
          TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam in ('aantal', 'aantalGroepen')"
      )
    skip_if_not(nrow(av) > 0, "aantal komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele == "Geheel getal")
    av_leeg <- av %>%
      filter(TypeVariabele != "Geheel getal")
    expect_equal(
      nrow(av_ok),
      2
    )
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_ok$Id, collapse = "', '")
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
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele bedekking bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
          TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekkingtest'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele aandeel bevat percentages", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeel'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele bedekkingExcl bevat percentages", {
    skip_on_cran()
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekkingExcl'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele bedekkingLaag bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekkingLaag'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele bedekkingLaagExcl bevat percentages en categorische var", { #nolint
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekkingLaagExcl'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele bedekkingLaagPlus bevat percentages en categorische var", { #nolint
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekkingLaagPlus'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele aandeelKruidlaag bevat percentages", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeelKruidlaag'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele bedekkingSom bevat percentages", {
    skip_on_cran()
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekkingSom'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele maxBedekking bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekking'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele maxBedekkingExcl bevat percentages en categorische var"
     , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekkingExcl'"
      )
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabele maxBedekking2s bevat percentages en categorische var", {
     ConnectieLSVIhabitats <-
       connecteerMetLSVIdb()
     AV <-
       dbGetQuery(
         ConnectieLSVIhabitats,
         "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
         TypeVariabele.Naam as TypeVariabele
         FROM AnalyseVariabele INNER JOIN TypeVariabele
         ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
         WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekking2s'"
       )
     av_leeg <- AV %>%
       filter(!TypeVariabele %in% c("Percentage", "Categorie"))
     FouteWaarden <-
       dbGetQuery(
         ConnectieLSVIhabitats,
         sprintf(
           "SELECT Id, Referentiewaarde FROM Voorwaarde
        WHERE AnalyseVariabeleId in ('%s')",
           paste(av_leeg$Id, collapse = "','")
         )
       )
     expect_equal(
       nrow(FouteWaarden),
       0
     )
     dbDisconnect(ConnectieLSVIhabitats)
   })

  it("AnalyseVariabele scoresom heeft typevariabele Geheel getal", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    AV <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
         TypeVariabele.Naam as TypeVariabele
         FROM AnalyseVariabele INNER JOIN TypeVariabele
         ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
         WHERE AnalyseVariabele.VariabeleNaam = 'scoresom'"
      )
    av_leeg <- AV %>%
      filter(TypeVariabele != "Geheel getal")
    FouteWaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Id, Referentiewaarde FROM Voorwaarde
        WHERE AnalyseVariabeleId in ('%s')",
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("De waarden van scoresom zijn getallen kleiner dan of gelijk aan 10 (als / 100)", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT vw.VoorwaardeNaam, vw.Referentiewaarde
        FROM Voorwaarde vw
        INNER JOIN AnalyseVariabele av ON vw.AnalyseVariabeleId = av.Id
        WHERE av.VariabeleNaam = 'scoresom'"
      )
    Refwaarden <- Refwaarden %>%
      filter(!.data$Referentiewaarde %in% Refwaarden$VoorwaardeNaam)
    expect_true(
      all(as.numeric(Refwaarden$Referentiewaarde) / 100 <= 10)
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("De waarden van percentages zijn getallen kleiner dan of gelijk aan 100", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT vw.VoorwaardeNaam, vw.Referentiewaarde
        FROM Voorwaarde vw
        INNER JOIN AnalyseVariabele av ON vw.AnalyseVariabeleId = av.Id
        INNER JOIN TypeVariabele tv ON av.TypeVariabeleId = tv.Id
        WHERE tv.Naam = 'Percentage'"
      )
    RefwaardenPerc <- RefwaardenPerc %>%
      filter(!.data$Referentiewaarde %in% RefwaardenPerc$VoorwaardeNaam)
    expect_true(
      all(as.numeric(RefwaardenPerc$Referentiewaarde) <= 100)
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("De waarden van categorische var zijn in de databank opgenomen schalen", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT vw.Referentiewaarde, vw.InvoermaskerId AS LijstId
        FROM Voorwaarde vw
        INNER JOIN AnalyseVariabele av ON vw.AnalyseVariabeleId = av.Id
        INNER JOIN TypeVariabele tv ON av.TypeVariabeleId = tv.Id
        WHERE tv.Naam = 'Categorie'"
      )
    LijstItems <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT LijstId, Waarde FROM LijstItem"
      )
    for (i in unique(RefwaardenCat$LijstId)) {
      expect_true(
        all(
          tolower((RefwaardenCat %>% filter(LijstId == i))$Referentiewaarde)
          %in% tolower((LijstItems %>% filter(LijstId == i))$Waarde)
        )
      )
    }
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabelen hebben telkens een SoortengroepId of StudiegroepId", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam in ('aantal', 'aantalGroepen',
          'aandeel', 'bedekking', 'bedekkingExcl', 'maxBedekking',
          'maxBedekking2s', 'maxBedekkingExcl', 'scoresom')"
      )
    skip_if_not(nrow(av) > 0, "AV komen niet voor")
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT TaxongroepId, StudiegroepId FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av$Id, collapse = "','")
        )
      )
    expect_true(
      all(!is.na(Refwaarden$TaxongroepId) | !is.na(Refwaarden$StudiegroepId))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabelen hebben telkens een SoortengroepId en StudiegroepId", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam in ('aandeelKruidlaag',
          'bedekkingLaag', 'bedekkingLaagExcl', 'bedekkingLaagPlus',
          'bedekkingSom')"
      )
    skip_if_not(nrow(av) > 0, "AV komen niet voor")
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT TaxongroepId, StudiegroepId FROM Voorwaarde
          WHERE AnalyseVariabeleId in ('%s')",
          paste(av$Id, collapse = "','")
        )
      )
    expect_true(
      all(!is.na(Refwaarden$TaxongroepId) & !is.na(Refwaarden$StudiegroepId))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AV bedekkingLaagExcl en bedekkingLaagPlus hebben 2 Soortengroepen", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    tg <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT vw.TaxongroepId, tgtg.TaxongroepChildId
        FROM Voorwaarde vw
          INNER JOIN AnalyseVariabele av ON vw.AnalyseVariabeleId = av.Id
          INNER JOIN TypeVariabele tv ON av.TypeVariabeleId = tv.Id
          LEFT JOIN TaxongroepTaxongroep tgtg
            ON vw.TaxongroepId = tgtg.TaxongroepParentId
        WHERE av.VariabeleNaam in ('bedekkingLaagExcl', 'bedekkingLaagPlus')"
      )
    skip_if_not(nrow(tg) > 0, "AV komen niet voor")
    Aantalgroepen <- tg %>%
      count(TaxongroepId)
    expect_true(
      all(Aantalgroepen$n == 2)
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("TypeVariabele Vrije tekst is nergens gebruikt", {
      ConnectieLSVIhabitats <-
        connecteerMetLSVIdb()
      av <-
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
        nrow(av),
        0
      )
      dbDisconnect(ConnectieLSVIhabitats)
    })

  it("Voor elke categorische variabele is een Invoermasker opgegeven", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
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
      all(!is.na(av$InvoermaskerId))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("Voor typevariabele scoresom is een Invoermasker opgegeven", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Voorwaarde.Id, Voorwaarde.InvoermaskerId,
        AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM Voorwaarde INNER JOIN AnalyseVariabele
          ON Voorwaarde.AnalyseVariabeleId = AnalyseVariabele.Id
        INNER JOIN TypeVariabele
          ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE TypeVariabele.Naam = 'scoresom'"
      )
    expect_true(
      all(!is.na(av$InvoermaskerId))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it(
    "De subanalysevariabele is overal correct ingevoerd (bedekking of aandeel)"
    , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Voorwaarde.Id, Voorwaarde.SubInvoermaskerId,
        Voorwaarde.SubReferentiewaarde,
        AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM Voorwaarde INNER JOIN AnalyseVariabele
        ON Voorwaarde.SubAnalyseVariabeleId = AnalyseVariabele.Id
        INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id"
      )
    lijst <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT LijstId, Waarde
        FROM Lijstitem"
      )
    expect_true(
      all(av$VariabeleNaam %in% c("bedekking", "aandeel"))
    )
    expect_true(
      all(av$TypeVariabele %in% c("Categorie", "Percentage"))
    )
    av_cat <- av %>%
      filter(.data$TypeVariabele == "Categorie")
    expect_true(
      all(!is.na(av_cat$SubInvoermaskerId))
    )
    expect_true(
      all(
        (av_cat %>%
          left_join(
            lijst, by = c("SubInvoermaskerId" = "LijstId"),
            relationship = "many-to-many"
          ) %>%
          group_by(Id) %>%
          summarise(
            test =
              max(grepl(tolower(unique(SubReferentiewaarde)), tolower(Waarde)))
          ) %>%
          ungroup())$test)
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("De subanalysevariabele is enkel gebruikt bij AnalyseVariabelen die dit ondersteunen", { #nolint
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT vw.Id, av.VariabeleNaam,
      sav.VariabeleNaam AS SubVariabeleNaam
      FROM Voorwaarde vw INNER JOIN AnalyseVariabele sav
      ON vw.SubAnalyseVariabeleId = sav.Id
      INNER JOIN Analysevariabele av
      ON vw.AnalyseVariabeleId = av.Id"
      )
    expect_true(
      all(av$VariabeleNaam %in%
            c("aantal", "aandeel", "aandeelKruidlaag", "bedekking",
              "bedekkingExcl", "maxBedekking", "maxBedekking2s",
              "maxBedekkingExcl"))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("Operator '=' is niet gebruikt tenzij bij type 'ja/nee'", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT Voorwaarde.Id, TypeVariabele.Naam as TypeVariabele,
          Voorwaarde.Operator
        FROM Voorwaarde INNER JOIN AnalyseVariabele
        ON Voorwaarde.SubAnalyseVariabeleId = AnalyseVariabele.Id
        INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE TypeVariabele.Naam != 'Ja/nee'
        AND Voorwaarde.Operator = '='"
      )
    expect_equal(
      nrow(av),
      0
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("Een indicator is een combinatie van AND, OR en voorwaardeID's", {
    ConnectieLSVIhabitats <- connecteerMetLSVIdb()
    Fouteformule <-
      geefInvoervereisten(ConnectieLSVIhabitats = ConnectieLSVIhabitats) %>%
      mutate(
        Formuletest = str_replace_all(.data$Combinatie, "\\(", ""),
        Formuletest = str_replace_all(.data$Formuletest, "\\)", "")
      ) %>%
      filter(
        str_detect(
          .data$Formuletest, "^(\\d+(( (AND|OR|<=|<|>|>=) \\d+))*)$"
        ) == FALSE
      )
    expect_equal(nrow(Fouteformule), 0)
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("Elke taxonnaam heeft 1 unieke nbn-key", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    Taxons <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT t.NbnTaxonVersionKey AS tkey,
          t.FloraNaamWetenschappelijk AS tflorawet,
          t.FloraNaamNederlands AS tfloranl,
          t.NbnNaam AS tnbn, t.NbnNaamVolledig AS tnbnvol,
          ts.NbnTaxonVersionKey AS tskey,
          ts.FloraNaamWetenschappelijk AS tsflorawet,
          ts.FloraNaamNederlands AS tsfloranl,
          ts.NbnNaam AS tsnbn, ts.NbnNaamVolledig AS tsnbnvol,
          ts.CanonicalNameWithMarker AS canname
        FROM Taxon t LEFT JOIN TaxonSynoniem ts
        ON t.Id = ts.TaxonId"
      )
    dbDisconnect(ConnectieLSVIhabitats)
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tflorawet) %>%
          distinct() %>%
          group_by(tflorawet) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tfloranl) %>%
          distinct() %>%
          group_by(tfloranl) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tnbn) %>%
          distinct() %>%
          group_by(tnbn) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tnbnvol) %>%
          distinct() %>%
          group_by(tnbnvol) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tsflorawet) %>%
          distinct() %>%
          group_by(tsflorawet) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tsfloranl) %>%
          distinct() %>%
          group_by(tsfloranl) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tsnbn) %>%
          distinct() %>%
          group_by(tsnbn) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, tsnbnvol) %>%
          distinct() %>%
          group_by(tsnbnvol) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tskey, tsflorawet) %>%
          distinct() %>%
          group_by(tsflorawet) %>%
          count(tskey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tskey, tsfloranl) %>%
          distinct() %>%
          group_by(tsfloranl) %>%
          count(tskey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tskey, tsnbn) %>%
          distinct() %>%
          group_by(tsnbn) %>%
          count(tskey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tskey, tsnbnvol) %>%
          distinct() %>%
          group_by(tsnbnvol) %>%
          count(tskey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tkey, canname) %>%
          distinct() %>%
          group_by(canname) %>%
          count(tkey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          select(tskey, canname) %>%
          distinct() %>%
          group_by(canname) %>%
          count(tskey) %>%
          filter(n > 1)
      ),
      0
    )
  })

  it("Het theoretisch maximum (Maximumwaarde) is correct berekend", {
    TMbedekkingaandeel <-
      geefInvoervereisten(ConnectieLSVIhabitats = connecteerMetLSVIdb()) %>%
      filter(
        AnalyseVariabele %in%
          c("aandeel", "aandeelKruidlaag", "bedekking", "meting_perc") |
        grepl("bedekking", tolower(AnalyseVariabele))
      ) %>%
      filter(Maximumwaarde != 1)
    expect_equal(nrow(TMbedekkingaandeel), 0)
    TMaantal <-
      geefInvoervereisten(ConnectieLSVIhabitats = connecteerMetLSVIdb()) %>%
      filter(
        AnalyseVariabele %in% c("aantal", "aantalGroepen")
      ) %>%
      filter(
        !(Maximumwaarde == 3 * as.numeric(sub(",", ".", Referentiewaarde)))
      ) %>%
      rowwise() %>%
      mutate(
        AantalSoortenKenmerken =
          ifelse(
            !is.na(TaxongroepId),
            nrow(
              geefSoortenlijstVoorIDs(
                as.character(TaxongroepId),
                ConnectieLSVIhabitats = connecteerMetLSVIdb()
              )
            ),
            str_count(Studiewaarde, ",") + 1
          )
      ) %>%
      filter(Maximumwaarde != AantalSoortenKenmerken)
    expect_equal(nrow(TMaantal), 0)
    TMmeting <-
      geefInvoervereisten(ConnectieLSVIhabitats = connecteerMetLSVIdb()) %>%
      filter(
        grepl("meting", AnalyseVariabele) &
          !AnalyseVariabele %in% c("meting_perc", "meting_bedekking"),
        !(TypeVariabele == "Ja/nee" & Maximumwaarde == 1),
        !(Voorwaarde == "aantal geslachten" & Maximumwaarde == 2),
        !(Voorwaarde == "bosconstantie" & Maximumwaarde == 250)
      ) %>%
      filter(
        !(Maximumwaarde == 3 * as.numeric(sub(",", ".", Referentiewaarde)))
      )
    expect_equal(nrow(TMmeting), 0)
  })
})
