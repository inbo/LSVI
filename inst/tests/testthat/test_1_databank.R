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
              "maxBedekking2s")
      )
    )
  })

  it("AnalyseVariabele aantal bevat enkel gehele getallen", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
          TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aantal'"
      )
    skip_if_not(nrow(av) > 0, "aantal komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele == "Geheel getal")
    av_leeg <- av %>%
      filter(TypeVariabele != "Geheel getal")
    expect_equal(
      nrow(av_ok),
      1
    )
    Refwaarden <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          av_ok$Id
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
  })

  it("AnalyseVariabele bedekking bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
          TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekking'"
      )
    skip_if_not(nrow(av) > 0, "bedekking komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage", "Categorie"))
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT VoorwaardeNaam, Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Percentage"))$Id
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
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    skip_if_not(nrow(av_ok) == 2, "Geen categorische var voor bedekking")
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Categorie"))$Id
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
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeel'"
      )
    skip_if_not(nrow(av) > 0, "aandeel komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage"))
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Percentage"))$Id
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
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
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
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage"))
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
  })

  it("AnalyseVariabele aandeelKruidlaag bevat percentages", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aandeelKruidlaag'"
      )
    skip_if_not(nrow(av) > 0, "aandeelKruidlaag komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage"))
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Percentage"))$Id
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
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
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
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage"))
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
  })

  it("AnalyseVariabele maxBedekking bevat percentages en categorische var", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekking'"
      )
    skip_if_not(nrow(av) > 0, "maxBedekking komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage", "Categorie"))
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Percentage"))$Id
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
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    skip_if_not(nrow(av_ok) == 2, "Geen categorische var voor maxBedekking")
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Categorie"))$Id
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
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekkingExcl'"
      )
    skip_if_not(nrow(av) > 0, "maxBedekkingExcl komt niet voor")
    av_ok <- av %>%
      filter(TypeVariabele %in% c("Percentage", "Categorie"))
    av_leeg <- av %>%
      filter(!TypeVariabele %in% c("Percentage", "Categorie"))
    RefwaardenPerc <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Percentage"))$Id
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
          paste(av_leeg$Id, collapse = "','")
        )
      )
    expect_equal(
      nrow(FouteWaarden),
      0
    )
    skip_if_not(nrow(av_ok) == 2, "Geen categorische var voor maxBedekkingExcl")
    RefwaardenCat <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(
          "SELECT Referentiewaarde FROM Voorwaarde
          WHERE AnalyseVariabeleId = '%s'",
          (av %>% filter(TypeVariabele == "Categorie"))$Id
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

  it("AnalyseVariabele maxBedekking2s bevat percentages en categorische var"
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
           WHERE AnalyseVariabele.VariabeleNaam = 'maxBedekking2s'"
         )
       skip_if_not(nrow(AV) > 0, "maxBedekking2s komt niet voor")
       av_ok <- AV %>%
         filter(TypeVariabele %in% c("Percentage", "Categorie"))
       av_leeg <- AV %>%
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
             paste(av_leeg$Id, collapse = "','")
           )
         )
       expect_equal(
         nrow(FouteWaarden),
         0
       )
       skip_if_not(
         nrow(av_ok) == 2, "Geen categorische var voor maxBedekking2s"
        )
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
    av <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'aantal'"
      )
    skip_if_not(nrow(av) > 0, "aantal komt niet voor")
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
  })

  it(
    "AnalyseVariabele bedekking heeft telkens een SoortengroepId of StudiegroepId" #nolint
    , {
      ConnectieLSVIhabitats <-
        connecteerMetLSVIlite()
      av <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          "SELECT AnalyseVariabele.Id, AnalyseVariabele.VariabeleNaam,
        TypeVariabele.Naam as TypeVariabele
        FROM AnalyseVariabele INNER JOIN TypeVariabele
        ON AnalyseVariabele.TypeVariabeleId = TypeVariabele.Id
        WHERE AnalyseVariabele.VariabeleNaam = 'bedekking'"
        )
      skip_if_not(nrow(av) > 0, "bedekking komt niet voor")
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
    })

  it("TypeVariabele Vrije tekst is nergens gebruikt", {
      ConnectieLSVIhabitats <-
        connecteerMetLSVIlite()
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
    })

  it("Voor elke categorische variabele is een Invoermasker opgegeven", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
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
  })

  it(
    "De subanalysevariabele is overal correct ingevoerd (bedekking of aandeel)"
    , {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
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
          left_join(lijst, by = c("SubInvoermaskerId" = "LijstId")) %>%
          group_by(Id) %>%
          summarise(
            test =
              max(grepl(tolower(unique(SubReferentiewaarde)), tolower(Waarde)))
          ) %>%
          ungroup())$test)
    )
  })

  it("De subanalysevariabele is enkel gebruikt bij AnalyseVariabele aantal", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
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
      all(av$VariabeleNaam == "aantal")
    )
  })

  it("Operator '=' is niet gebruikt tenzij bij type 'ja/nee'", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
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
  })

  it("Elke taxonnaam heeft 1 unieke nbn-key", {
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
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

})
