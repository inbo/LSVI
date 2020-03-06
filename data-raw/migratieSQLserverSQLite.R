# Deze functie zorgt voor de migratie van (een deel van) de databank op SQL-server naar een SQLite-databank in het package zelf in de map inst/data

library(DBI)
library(RSQLite)
library(LSVI)
library(dplyr)
library(purrr)

migratieSQLserverSQLite <-
  function(
    #VectorHabitattypes = c("1330_hpr", "4030", "9130", "9190", "91E0_va"),
    #Versie = "Versie 2.0"
  ) {
  #Tabellen ophalen uit SQLserver
  Habitatgroep <-
    dbGetQuery(ConnectiePool, "SELECT Id, Naam FROM Habitatgroep")
  
  Habitattype <-        #Hier zitten enkele lege velden bij, en enkele die mogelijk overbodig zijn, nog na te kijken!
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, Code, Naam, NaamKort, HabitatgroepId, ParentId,
      cast(Omschrijving AS nvarchar(10)) AS Omschrijving,
      cast(Toelichting AS nvarchar(350)) AS Toelichting,
      cast(Referentie AS nvarchar(30)) AS Referentie,
      cast(Opmerking AS nvarchar(400)) AS Opmerking, GroepVrij
      FROM Habitattype"
    )
  
  HabitattypeId <- (Habitattype %>%
    #filter(Code %in% VectorHabitattypes) %>%
    summarise(Id = paste0(Id, collapse = ",")))$Id
  
  Versie <-
    dbGetQuery(
      ConnectiePool,
      #sprintf(
        "SELECT Id, VersieLSVI,
        cast(Referentie AS nvarchar(30)) AS Referentie,
        cast(Beschrijving AS nvarchar(120)) AS Beschrijving,
        Kwaliteitsniveau1, Kwaliteitsniveau2
        FROM Versie"
      #)
    )
  
  VersieId <- (Versie %>% summarise(Id = paste0(Id, collapse = ",")))$Id
  
  Criterium <-
    dbGetQuery(ConnectiePool, "SELECT Id, Naam FROM Criterium")
  
  Indicator <-
    dbGetQuery(ConnectiePool, "SELECT Id, CriteriumId, Naam FROM Indicator")
  
  Indicator_habitat <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, IndicatorId,
        cast(Beschrijving AS nvarchar(1050)) AS Beschrijving,
        cast(Beschrijving_naSoorten AS nvarchar(200)) AS Beschrijving_naSoorten,
        cast(Maatregelen AS nvarchar(510)) AS Maatregelen,
        cast(Opmerkingen AS nvarchar(830)) AS Opmerkingen,
        cast(Referenties AS nvarchar(290)) AS Referenties,
        TaxongroepId, HabitattypeId, VersieId
        FROM Indicator_habitat
        WHERE HabitattypeId in (%s) and VersieId in (%s)",
        HabitattypeId, VersieId
      )
    )
  
  Indicator_habitatId <-
    (Indicator_habitat %>% summarise(Id = paste0(Id, collapse = ",")))$Id
  
  IndicatortabellenKoppeling <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, Indicator_habitatId, Indicator_beoordelingId
        FROM IndicatortabellenKoppeling
        WHERE Indicator_habitatId in (%s)",
        Indicator_habitatId
      )
    )
  
  Indicator_beoordelingId <-
    (IndicatortabellenKoppeling %>%
       summarise(Id = paste0(Indicator_beoordelingId, collapse = ",")))$Id
  
  Indicator_beoordeling <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, IndicatorId, HabitattypeId, VersieId,
        cast(Opmerkingen AS nvarchar(900)) AS Opmerkingen,
        cast(Referenties AS nvarchar(150)) AS Referenties, Belang
        FROM Indicator_beoordeling
        WHERE Id in (%s)",
        Indicator_beoordelingId
      )
    )
  
  Beoordeling <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, Indicator_beoordelingId, Kwaliteitsniveau,
        cast(Beoordeling_letterlijk AS nvarchar(360)) AS Beoordeling_letterlijk
        FROM Beoordeling
        WHERE Indicator_beoordelingId in (%s)",
        Indicator_beoordelingId
      )
    )
  
  BeoordelingId <-
    (Beoordeling %>% summarise(Id = paste0(Id, collapse = ",")))$Id
  
  CombinerenVoorwaarden <-         #BewerkingAND niet overgenomen, die mag eigenlijk ook weg in de brondb!
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, BeoordelingId, VoorwaardeID1, VoorwaardeID2,
        ChildID1, ChildID2, BewerkingOperator
        FROM CombinerenVoorwaarden
        WHERE BeoordelingId in (%s)",
        BeoordelingId
      )
    )
  
  VoorwaardeId <-
    paste0(
      unique(
        c(
          (CombinerenVoorwaarden %>% filter(!is.na(VoorwaardeID1)))$VoorwaardeID1,
          (CombinerenVoorwaarden %>% filter(!is.na(VoorwaardeID2)))$VoorwaardeID2
        )
      ),
      collapse = ","
    )
  
  Voorwaarde <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, VoorwaardeNaam, ExtraInfo,
        AnalyseVariabeleId, Referentiewaarde, Operator, InvoermaskerId,
        TaxongroepId, StudiegroepId, SubAnalyseVariabeleId,
        SubReferentiewaarde, SubOperator, SubInvoermaskerId
        FROM Voorwaarde
        WHERE Id in (%s)",
        VoorwaardeId
      )
    )
  
  Lijst <- dbGetQuery(ConnectiePool, "SELECT Id, Naam FROM Lijst")
  
  LijstItem <-
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, LijstId, Waarde, Volgnummer, Omschrijving, Ondergrens,
      Gemiddelde, Bovengrens, Basisschaal FROM LijstItem"
    ) %>%
    filter(!is.na(.data$Ondergrens)) %>%  #voorwaarden zonder onder- en bovengrens er voorlopig uit halen!
    bind_rows(
      data.frame(
        Id = 8, LijstId = 1, Waarde = "lf", Ondergrens = 2, Gemiddelde = 3,
        Bovengrens = 5, Basisschaal = FALSE, stringsAsFactors = FALSE
      )
    )
  
  StudiegroepId <-
    paste0(
      unique(
        (Voorwaarde %>% filter(!is.na(StudiegroepId)))$StudiegroepId
      ),
      collapse = ","
    )
  
  Studiegroep <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, Naam, LijstNaam
        FROM Studiegroep
        WHERE Id in (%s)",
        StudiegroepId
      )
    )
  
  StudieItem <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, StudiegroepId, Waarde, Volgnummer, Omschrijving,
        Ondergrens, Gemiddelde, Bovengrens
        FROM StudieItem
        WHERE StudiegroepId in (%s)",
        StudiegroepId
      )
    )
  
  AnalyseVariabeleId <-
    paste0(
      unique(
        (Voorwaarde %>% filter(!is.na(AnalyseVariabeleId)))$AnalyseVariabeleId
      ),
      collapse = ","
    )
  
  AnalyseVariabele <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, VariabeleNaam, Eenheid, TypeVariabeleId
        FROM AnalyseVariabele
        WHERE Id in (%s)",
        AnalyseVariabeleId
      )
    )
  
  TypeVariabele <-
    dbGetQuery(ConnectiePool, "SELECT Id, Naam from TypeVariabele")
  
  TaxongroepId <-
    paste0(
      unique(
        c(
          (Voorwaarde %>% filter(!is.na(TaxongroepId)))$TaxongroepId,
          (Indicator_habitat %>% filter(!is.na(TaxongroepId)))$TaxongroepId
        )
      ),
      collapse = ","
    )
  
  TaxongroepTaxongroep <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "WITH Taxongroeplijst
        AS
        (
          SELECT T1.Id, T1.TaxongroepParentId, T1.TaxongroepChildId
          FROM TaxongroepTaxongroep T1
          WHERE T1.TaxongroepParentId in (%s)
        UNION ALL
          SELECT T2.Id, T2.TaxongroepParentId, T2.TaxongroepChildId
          FROM TaxongroepTaxongroep T2 INNER JOIN Taxongroeplijst T
          ON T2.TaxongroepParentId = T.TaxongroepChildId
        )
        SELECT * FROM Taxongroeplijst
        ",
        TaxongroepId
      )
    )
  
  TaxongroepId <-
    paste0(
      c(
        TaxongroepId,
        unique(TaxongroepTaxongroep$TaxongroepChildId)
      ),
      collapse = ","
    )
  
  Taxongroep <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, Naam,
        cast(Omschrijving AS nvarchar(90)) AS Omschrijving, AfkomstGegevens
        FROM Taxongroep
        WHERE Id in (%s)",
        TaxongroepId
      )
    )
  
  TaxongroepTaxon <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, TaxongroepId, TaxonId
        FROM TaxongroepTaxon
        WHERE TaxongroepId in (%s)",
        TaxongroepId
      )
    )
  
  Taxon <-
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, NbnTaxonVersionKey, FloraNaamWetenschappelijk,
      FloraNaamNederlands, FloraTaxonId, FloraCode, TaxonTypeId,
      NbnNaam, NbnNaamVolledig, NbnTaal
      FROM Taxon"
    )
  
  TaxonTaxon <-
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, TaxonParentId, TaxonChildId
      FROM TaxonTaxon"
    )
  
  TaxonSynoniem <-               #aanpassing Gbif-namen!!!  Na definitieve migratie ook in brondb aanpassen?
    dbGetQuery(                  #(als het de gebruikers niet meer hindert als ze de kopie gebruiken)
      ConnectiePool,
      "SELECT Id, NBNTaxonVersionKey, FloraNaamWetenschappelijk,
      FloraNaamNederlands, FloraTaxonId, FloraCode, TaxonTypeId,
      NbnNaam, NbnNaamVolledig, NbnTaal,
      GbifCanonicalName AS CanonicalName,
      GbifCanonicalNameWithMarker AS CanonicalNameWithMarker,
      GbifCanonicalNameComplete AS CanonicalNameComplete,
      TaxonId
      FROM TaxonSynoniem;"
    )
  
  TaxonType <-
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, Naam
      FROM TaxonType"
    )


  #berekening Theoretisch Maximum
  Voorwaarde <- Voorwaarde %>%
    rowwise() %>%
    mutate(
      AantalSoorten =
        ifelse(
          !is.na(TaxongroepId),
          nrow(geefSoortenlijstVoorIDs(as.character(TaxongroepId))),
          NA
        )
    ) %>%
    ungroup() %>%
    left_join(StudieItem, by = "StudiegroepId", suffix = c("", ".studie")) %>%
    group_by(
      Id, VoorwaardeNaam, ExtraInfo, AnalyseVariabeleId, Referentiewaarde,
      Operator, InvoermaskerId, TaxongroepId, StudiegroepId,
      SubAnalyseVariabeleId, SubReferentiewaarde, SubOperator,
      SubInvoermaskerId, AantalSoorten
    ) %>%
    summarise(AantalKenmerken = n()) %>%
    ungroup() %>%
    inner_join(AnalyseVariabele, by = c("AnalyseVariabeleId" = "Id")) %>%
    inner_join(TypeVariabele, by = c("TypeVariabeleId" = "Id")) %>%
    mutate(
      Maximumwaarde =
        ifelse(
          VariabeleNaam %in% c("aandeel", "aandeelKruidlaag", "meting_perc"),
          1,
          NA
        ),
      Maximumwaarde =
        ifelse(
          grepl("bedekking", tolower(VariabeleNaam)), 1, Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", VariabeleNaam) & Naam == "Categorie", 1, Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", VariabeleNaam) & Naam == "Ja/nee", 1, Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", VariabeleNaam) & VoorwaardeNaam == "aantal geslachten",
          2, Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", VariabeleNaam) & VoorwaardeNaam == "bosconstantie",
          250, Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          VariabeleNaam == "aantal" & !is.na(TaxongroepId),
          AantalSoorten,
          Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          VariabeleNaam == "aantal" & is.na(TaxongroepId) & !is.na(StudiegroepId),
          AantalKenmerken,
          Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", VariabeleNaam) & Naam != "Ja/nee" | VariabeleNaam == "aantal",
          pmin(3 * as.numeric(sub(",", ".", Referentiewaarde)), Maximumwaarde,
               na.rm = TRUE),
          Maximumwaarde
        )
    ) %>%
    select(
      Id, VoorwaardeNaam, ExtraInfo, AnalyseVariabeleId, Referentiewaarde,
      Operator, InvoermaskerId, TaxongroepId, StudiegroepId,
      SubAnalyseVariabeleId, SubReferentiewaarde, SubOperator,
      SubInvoermaskerId, Maximumwaarde
    )
  
  NieuweDb <- dbConnect(SQLite(), "inst/databank/LSVIHabitatTypes.sqlite")
  dbWriteTable(NieuweDb, "AnalyseVariabele", AnalyseVariabele)
  dbWriteTable(NieuweDb, "Beoordeling", Beoordeling)
  dbWriteTable(NieuweDb, "CombinerenVoorwaarden", CombinerenVoorwaarden)
  dbWriteTable(NieuweDb, "Criterium", Criterium)
  dbWriteTable(NieuweDb, "Habitatgroep", Habitatgroep)
  dbWriteTable(NieuweDb, "Habitattype", Habitattype)
  dbWriteTable(NieuweDb, "Indicator", Indicator)
  dbWriteTable(NieuweDb, "Indicator_beoordeling", Indicator_beoordeling)
  dbWriteTable(NieuweDb, "Indicator_habitat", Indicator_habitat)
  dbWriteTable(NieuweDb, "IndicatortabellenKoppeling", IndicatortabellenKoppeling)
  dbWriteTable(NieuweDb, "Lijst", Lijst)
  dbWriteTable(NieuweDb, "LijstItem", LijstItem)
  dbWriteTable(NieuweDb, "Studiegroep", Studiegroep)
  dbWriteTable(NieuweDb, "StudieItem", StudieItem)
  dbWriteTable(NieuweDb, "Taxon", Taxon)
  dbWriteTable(NieuweDb, "Taxongroep", Taxongroep)
  dbWriteTable(NieuweDb, "TaxongroepTaxon", TaxongroepTaxon)
  dbWriteTable(NieuweDb, "TaxongroepTaxongroep", TaxongroepTaxongroep)
  dbWriteTable(NieuweDb, "TaxonSynoniem", TaxonSynoniem)
  dbWriteTable(NieuweDb, "TaxonTaxon", TaxonTaxon)
  dbWriteTable(NieuweDb, "TaxonType", TaxonType)
  dbWriteTable(NieuweDb, "TypeVariabele", TypeVariabele)
  dbWriteTable(NieuweDb, "Versie", Versie)
  dbWriteTable(NieuweDb, "Voorwaarde", Voorwaarde)
  dbDisconnect(NieuweDb)
}

#databank wissen
unlink("inst/databank/LSVIHabitatTypes.sqlite")

#databank terug vullen
LSVI:::maakConnectiePoolServer()
migratieSQLserverSQLite()
