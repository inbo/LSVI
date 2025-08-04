context("test databank")

library(DBI)
library(dplyr)
library(stringr)
library(readr)

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

  it("In de databank zitten enkel AnalyseVariabelen waarvoor code ontwikkeld is", { #nolint: line_length_linter
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT DISTINCT(VariabeleNaam) FROM AnalyseVariabele"
      ) %>%
      filter(!grepl("^meting", .data$VariabeleNaam))
    expect_true(
      all(
        av$VariabeleNaam %in%
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
        WHERE AnalyseVariabele.VariabeleNaam in ('aantal', 'aantalGroepen',
          'scoresom')"
      )
    skip_if_not(nrow(av) > 0, "aantal komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele == "Geheel getal")
    av_leeg <- av %>%
      filter(TypeVariabele != "Geheel getal")
    expect_equal(
      nrow(av_ok),
      3
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

  it("AnalyseVariabele bedekkingLaagExcl bevat percentages en categorische var", { #nolint: line_length_linter
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

  it("AnalyseVariabele bedekkingLaagPlus bevat percentages en categorische var", { #nolint: line_length_linter
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

  it("AnalyseVariabele maxBedekkingExcl bevat percentages en categorische var" , { #nolint: line_length_linter
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

  it("De waarden van scoresom zijn getallen kleiner dan of gelijk aan 10 (als / 100)", { #nolint: line_length_linter
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

  it("AnalyseVariabelen hebben telkens een TaxonGroepCode of StudiegroepId", {
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
          "SELECT vwtg.TaxonGroepCode, vw.StudiegroepId
          FROM Voorwaarde vw
            LEFT JOIN VoorwaardeTaxonGroep vwtg ON vw.Id = vwtg.VoorwaardeId
          WHERE vw.AnalyseVariabeleId in ('%s')",
          paste(av$Id, collapse = "','")
        )
      )
    expect_true(
      all(!is.na(Refwaarden$TaxonGroepCode) | !is.na(Refwaarden$StudiegroepId))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AnalyseVariabelen hebben telkens een TaxonGroepCode en StudiegroepId", {
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
          "SELECT vwtg.TaxonGroepCode, vw.StudiegroepId
          FROM Voorwaarde vw
            LEFT JOIN VoorwaardeTaxonGroep vwtg ON vw.Id = vwtg.VoorwaardeId
          WHERE vw.AnalyseVariabeleId in ('%s')",
          paste(av$Id, collapse = "','")
        )
      )
    expect_true(
      all(!is.na(Refwaarden$TaxonGroepCode) & !is.na(Refwaarden$StudiegroepId))
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("AV bedekkingLaagExcl en bedekkingLaagPlus hebben 2 Soortengroepen", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    tg <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT vwtg.VoorwaardeId, vwtg.TaxonGroepCode
        FROM Voorwaarde vw
          INNER JOIN AnalyseVariabele av ON vw.AnalyseVariabeleId = av.Id
          INNER JOIN TypeVariabele tv ON av.TypeVariabeleId = tv.Id
          LEFT JOIN VoorwaardeTaxonGroep vwtg ON vw.Id = vwtg.VoorwaardeId
        WHERE av.VariabeleNaam in ('bedekkingLaagExcl', 'bedekkingLaagPlus')"
      )
    skip_if_not(nrow(tg) > 0, "AV komen niet voor")
    Aantalgroepen <- tg %>%
      count(VoorwaardeId)
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

  it("De subanalysevariabele is overal correct ingevoerd (bedekking of aandeel)", { #nolint: line_length_linter
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
          ungroup())$test
      )
    )
    dbDisconnect(ConnectieLSVIhabitats)
  })

  it("De subanalysevariabele is enkel gebruikt bij AnalyseVariabelen die dit ondersteunen", { #nolint: line_length_linter
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

  it("Elke taxonnaam heeft 1 unieke GbifUsageKey (in ObservatieTaxon)", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    Taxons <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT TaxonName, GbifUsageKey, NaamNederlands, NbnTaxonVersionKey,
          GbifKeyTaxonNaam
        FROM ObservatieTaxon"
      )
    dbDisconnect(ConnectieLSVIhabitats)
    expect_equal(
      nrow(
        Taxons %>%
          distinct(TaxonName, GbifUsageKey) %>%
          group_by(TaxonName) %>%
          count(GbifUsageKey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(NaamNederlands, GbifUsageKey) %>%
          group_by(NaamNederlands) %>%
          count(GbifUsageKey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(NbnTaxonVersionKey, GbifUsageKey) %>%
          group_by(NbnTaxonVersionKey) %>%
          count(GbifUsageKey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(GbifKeyTaxonNaam, GbifUsageKey) %>%
          group_by(GbifKeyTaxonNaam) %>%
          count(GbifUsageKey) %>%
          filter(n > 1)
      ),
      0
    )
    expect_true(all(Taxons$GbifUsageKey %in% Taxons$GbifKeyTaxonNaam))
  })

  it("Elke GbifUsageKey van ObservatieTaxon staat ook in Taxon", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    ConnectedTaxons <- dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT ot.TaxonName, ot.GbifUsageKey
      FROM ObservatieTaxon ot
      LEFT JOIN Taxon t ON ot.GbifUsageKey = t.GbifUsageKey
      WHERE t.Rank IS NULL"
    )
    dbDisconnect(ConnectieLSVIhabitats)
    expect_equal(
      nrow(ConnectedTaxons),
      0
    )
  })

  it("Elke taxonnaam heeft een GbifUsageKey en Rank", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    Taxons <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT ot.TaxonName, ot.NaamNederlands, ot.NbnTaxonVersionKey,
          ot.GbifKeyTaxonNaam, t.GbifUsageKey, t.Rank
        FROM Taxon t LEFT JOIN ObservatieTaxon ot
        ON t.GbifUsageKey = ot.GbifUsageKey"
      )
    dbDisconnect(ConnectieLSVIhabitats)
    expect_equal(
      nrow(
        Taxons %>%
          filter(is.na(GbifUsageKey))
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(TaxonName, GbifUsageKey, Rank) %>%
          filter(!is.na(TaxonName)) %>%
          group_by(TaxonName) %>%
          filter(is.na(GbifUsageKey) | is.na(Rank))
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(NaamNederlands, GbifUsageKey, Rank) %>%
          filter(!is.na(NaamNederlands)) %>%
          group_by(NaamNederlands) %>%
          filter(is.na(GbifUsageKey) | is.na(Rank))
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(NbnTaxonVersionKey, GbifUsageKey, Rank) %>%
          filter(!is.na(NbnTaxonVersionKey)) %>%
          group_by(NbnTaxonVersionKey) %>%
          filter(is.na(GbifUsageKey) | is.na(Rank))
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(GbifKeyTaxonNaam, GbifUsageKey, Rank) %>%
          filter(!is.na(GbifKeyTaxonNaam)) %>%
          group_by(GbifKeyTaxonNaam) %>%
          filter(is.na(GbifUsageKey) | is.na(Rank))
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          distinct(GbifUsageKey, Rank) %>%
          group_by(GbifUsageKey) %>%
          count(Rank) %>%
          filter(n > 1)
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          filter(is.na(Rank))
      ),
      0
    )
    expect_equal(
      nrow(
        Taxons %>%
          filter(is.na(GbifUsageKey))
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
      group_by(VoorwaardeID) %>%
      mutate(
        AantalSoortenKenmerken = ifelse(
          !is.na(max(TaxonGroepCode)),
          length(
            unique(
              (
                geefSoortenlijstVoorIDs(
                  as.character(TaxonGroepCode),
                  ConnectieLSVIhabitats = connecteerMetLSVIdb()
                )
              )$GbifUsageKey
            )
          ),
          str_count(Studiewaarde, ",") + 1
        )
      ) %>%
      ungroup() %>%
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
  it("Functie logDatabankfouten() geeft nog problemen", {
    Databankfouten <-
      logDatabankfouten(ConnectieLSVIhabitats = connecteerMetLSVIdb())
    expect_equal(nrow(Databankfouten[[1]]), 0)
    expect_equal(nrow(Databankfouten[[2]]), 0)
  })
})

describe("test tabellen Taxon en Observatietaxon", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  Taxonlijst <- dbGetQuery(
    ConnectieLSVIhabitats,
    "SELECT t.GbifUsageKey, ot.TaxonName, ot.NaamNederlands,
        ot.NbnTaxonVersionKey, ot.GbifKeyTaxonNaam, t.Wetnaam, t.Rank,
        t.Kingdom, t.Phylum, t.[Order], t.Family, t.Genus, t.Species,
        t.KingdomKey, t.PhylumKey, t.ClassKey, t.OrderKey, t.FamilyKey,
        t.GenusKey, t.SpeciesKey
      FROM ObservatieTaxon ot
        RIGHT JOIN Taxon t on ot.GbifUsageKey = t.GbifUsageKey"
  )
  dbDisconnect(ConnectieLSVIhabitats)
  it("TaxonName is niet uniek", {
    expect_equal(
      Taxonlijst %>%
        distinct(GbifUsageKey, TaxonName) %>%
        count(TaxonName) %>%
        filter(!is.na(TaxonName), n > 1) %>%
        nrow(),
      0
    )
  })
  it("TaxonName bevat dubieuze naamgeving", {
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl("nom. rejec.", TaxonName)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl(" cf. ", TaxonName)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl(" auct. ", TaxonName)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl(" non ", TaxonName)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl("  ", TaxonName)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl(" \\+ ", TaxonName) & .data$Rank != "SPECIESGROUP"
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl(" gro[eu]p", TaxonName) & Rank != "SPECIESGROUP"
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          str_count(TaxonName, " ") == 0,
          !Rank %in% c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY"),
          !(Rank == "GENUS" & Kingdom == "Fungi"),
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          str_count(TaxonName, " ") != 0,
          str_count(TaxonName, " ") != str_count(TaxonName, " \\("),
          Rank %in% c("PHYLUM", "CLASS", "ORDER", "FAMILY")
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          str_count(TaxonName, " ") == 1,
          Rank != "GENUS",
          !(TaxonName == "Equisetum ×font-queri" & Rank == "SPECIES")
        ) %>%
        nrow(),
      0
    )
    trimTaxonName <- function(x) {
      x <- gsub(" non ", " non", x)
      x <- gsub(" \\(?[DdL][euo]n? ", " ", x)
      x <- gsub(" [Vv]an ", " van", x)
      x <- gsub(" & ", "", x)
      x <- gsub(", ", "", x)
      x <-
        gsub("Kerguélen ex Carreras Martínez", "KerguélenExCarrerasMartínez", x)
      x <- gsub(" [Ee]x ", "", x)
      x <- gsub(" [Ee]t ", "", x)
      x <- gsub("Haller f.", "Hallerf.", x)
      x <- gsub("Dalla Torre", "DallaTorre", x)
      x <- gsub("Zhao Xin", "ZhaoXin", x)
      x <- gsub("‘t Hart", "'tHart", x)
      x <- gsub("Ker Gawl.", "KerGawl.", x)
      x <- gsub("Romero Zarco", "RomeroZarco", x)
      x <- gsub(" Burman f.", " Burmanf.", x)
      x <- gsub("St John", "StJohn", x)
      x <- gsub(" Delise ex", " DeliseEx", x)
      x <- gsub(" Coppins J", "CoppinsJ", x)
      x <- gsub("Roth ([np])", "Roth//1", x)
      x <- gsub("Jouve ", "Jouve", x)
      x <- gsub("Ronse Decraene", "RonseDecraene", x)
      x <- gsub("Schmidt ", "Schmidt", x)
      x <- gsub("Hoffmann ", "Hoffmann", x)
      x <- gsub("Bellynck ", "Bellynck", x)
      x <- gsub("Porter ", "Porter", x)
      x <- gsub("Reichard ", "Reichard", x)
      x <- gsub("Wils.ex Mitt.", "Wils.exMitt.", x)
      x <- gsub("Pollich ", "Pollich", x)
      x <- gsub("Gross ", "Gross", x)
      x <- gsub("Poiret ", "Poiret", x)
      x <- gsub("Schulze ", "Schulze", x)
      x <- gsub("Weihe ", "Weihe", x)
      x <- gsub("Tausch ", "Tausch", x)
      x <- gsub("Mérat ", "Mérat", x)
      x <- gsub("Smith non", "Smithnon", x)
      x <- gsub("Krause ", "Krause", x)
      x <- gsub("Lasch ", "Lasch", x)
      x <- gsub(" an Edees", " anEdees", x)
      x <- gsub("Wachter ", "Wachter", x)
      x <- gsub("Sánchez Ocharan", "SánchezOcharan", x)
      x <- gsub("de Lesd", "deLesd", x)
      x <- gsub("Kuan Zhao", "KuanZhao", x)
      x <- gsub("Zhua L", "ZhuaL", x)
      x <- gsub("\\) ", "", x)
      x <- gsub("( subg\\. )", "\\1 ", x)
      x <- gsub("( subsp\\. )", "\\1 ", x)
      x <- gsub("( var\\. )", "\\1 ", x)
      x <- gsub("( f\\. )", "\\1 ", x)
      x <- gsub("(sect\\. )", "\\1 ", x)
      x <- gsub("\\. ", "", x)
      return(x)
    }
    expect_equal(
      Taxonlijst %>%
        mutate(
          TaxonNameTrimmed = trimTaxonName(TaxonName)
        ) %>%
        filter(
          str_count(TaxonNameTrimmed, " ") > 1,
          Rank == "GENUS"
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        mutate(
          TaxonNameTrimmed = trimTaxonName(TaxonName)
        ) %>%
        filter(
          str_count(TaxonNameTrimmed, " ") == 2,
          !Rank %in% c("SPECIES", "SUBGENUS")
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        mutate(
          TaxonNameTrimmed = trimTaxonName(TaxonName)
        ) %>%
        filter(
          str_count(TaxonNameTrimmed, " ") > 2,
          Rank == "SPECIES"
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        mutate(
          TaxonNameTrimmed = trimTaxonName(TaxonName)
        ) %>%
        filter(
          str_count(TaxonNameTrimmed, " ") == 4,
          !Rank %in% c("SUBSPECIES", "VARIETY", "FORM")
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        mutate(
          TaxonNameTrimmed = trimTaxonName(TaxonName)
        ) %>%
        filter(
          str_count(TaxonNameTrimmed, " ") != 4,
          Rank %in% c("SUBSPECIES", "VARIETY", "FORM")
        ) %>%
        nrow(),
      0
    )
  })
  it("NaamNederlands is niet uniek", {
    expect_equal(
      Taxonlijst %>%
        distinct(GbifUsageKey, NaamNederlands) %>%
        count(NaamNederlands) %>%
        filter(!is.na(NaamNederlands), n > 1) %>%
        nrow(),
      0
    )
  })
  it("NbnTaxonVersionKey is niet uniek", {
    expect_equal(
      Taxonlijst %>%
        distinct(GbifUsageKey, NbnTaxonVersionKey) %>%
        count(NbnTaxonVersionKey) %>%
        filter(!is.na(NbnTaxonVersionKey), n > 1) %>%
        nrow(),
      0
    )
  })
  it("GbifKeyTaxonNaam is niet uniek", {
    expect_equal(
      Taxonlijst %>%
        distinct(GbifUsageKey, GbifKeyTaxonNaam) %>%
        count(GbifKeyTaxonNaam) %>%
        filter(!is.na(GbifKeyTaxonNaam), n > 1) %>%
        nrow(),
      0
    )
  })
  it("Rank is niet overeenkomstig met de data", {
    expect_equal(
      Taxonlijst %>%
        filter(
          !is.na(Species),
          !Rank %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          is.na(Species),
          Rank %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM")
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          grepl("subsp\\.", TaxonName),
          Rank != "SUBSPECIES"
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(grepl("var\\.", TaxonName), Rank != "VARIETY") %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(grepl(" f\\. ", TaxonName), Rank != "FORM") %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          Rank %in%
            c("PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SUBGENUS"),
          !is.na(Species)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          !Rank %in%
            c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS",
              "SUBGENUS"),
          is.na(Species)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          Rank %in% c("PHYLUM", "CLASS", "ORDER", "FAMILY"),
          !is.na(Genus)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          !Rank %in% c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY"),
          is.na(Genus)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          Rank %in% c("PHYLUM", "CLASS", "ORDER"),
          !is.na(Family)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          !Rank %in% c("PHYLUM", "CLASS", "ORDER"),
          is.na(Family)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          Rank %in% c("PHYLUM", "CLASS"),
          !is.na(Order)
        ) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        filter(
          !Rank %in% c("PHYLUM", "CLASS"),
          is.na(Order)
        ) %>%
        nrow(),
      0
    )
  })
  it("Speciesnaam niet overeenkomstig tussen WetNaam en Species (in Taxon)", {
    expect_equal(
      Taxonlijst %>%
        filter(
          Rank %in% c("SPECIES", "SUBSPECIES", "VARIETY", "FORM"),
          gsub("^([^ ]+ [^ ]+)( .*)?", "\\1", Species) !=
            gsub("^([^ ]+ [^ ]+)( .*)?", "\\1", WetNaam)
        ) %>%
        nrow(),
      0
    )
  })
  it("Genusnaam is niet overeenkomstig tussen Genus en Species (in Taxon)", {
    expect_equal(
      Taxonlijst %>%
        filter(
          Rank %in% c("GENUS", "SPECIES", "SUBSPECIES", "VARIETY", "FORM"),
          gsub("^([^ ]+)( .*)?", "\\1", Genus) !=
            gsub("^([^ ]+)( .*)?", "\\1", Species)
        ) %>%
        nrow(),
      0
    )
  })
  it("Taxonlijst bevat geen dubbels (m.u.v. namen die synoniem zijn)", {
    expect_equal(
      Taxonlijst %>%
        select(
          -"TaxonName", -"NaamNederlands",
          -"NbnTaxonVersionKey"
        ) %>%
        distinct() %>%
        count(GbifUsageKey) %>%
        filter(n > 1) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        select(
          -"NaamNederlands", -"NbnTaxonVersionKey"
        ) %>%
        distinct() %>%
        count(TaxonName) %>%
        filter(n > 1) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        select(
          -"TaxonName", -"NaamNederlands"
        ) %>%
        distinct() %>%
        count(NbnTaxonVersionKey) %>%
        filter(n > 1, !is.na(NbnTaxonVersionKey)) %>%
        nrow(),
      0
    )
    expect_equal(
      Taxonlijst %>%
        mutate(NaamNederlands = tolower(NaamNederlands)) %>%
        select(
          -"TaxonName", -"NbnTaxonVersionKey"
        ) %>%
        distinct() %>%
        count(NaamNederlands) %>%
        filter(n > 1, !is.na(NaamNederlands)) %>%
        nrow(),
      0
    )
  })
})
