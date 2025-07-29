# Deze functie zorgt voor de migratie van (een deel van) de databank op
# SQL-server naar een SQLite-databank in het package zelf in map inst/databank

library(DBI)
library(RSQLite)
library(LSVI)
library(dplyr)
library(purrr)
library(inbodb)

migratieSQLserverSQLite <- function() {
  #Tabellen ophalen uit SQLserver
  Habitatgroep <-
    dbGetQuery(ConnectiePool, "SELECT Id, Naam FROM Habitatgroep")

  Habitattype <-
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, Code, Naam, NaamKort, HabitatgroepId, ParentId,
      cast(Omschrijving AS nvarchar(10)) AS Omschrijving,
      cast(Toelichting AS nvarchar(350)) AS Toelichting,
      cast(Referentie AS nvarchar(30)) AS Referentie,
      cast(Opmerking AS nvarchar(400)) AS Opmerking, GroepVrij
      FROM Habitattype"
    )   #Veld Omschrijving is overal leeg, misschien niet nodig om te behouden?

  HabitattypeId <-
    (Habitattype %>%
      summarise(Id = paste0(Id, collapse = ","))
    )$Id

  Versie <-
    dbGetQuery(
      ConnectiePool,
      "SELECT Id, VersieLSVI,
      cast(Referentie AS nvarchar(30)) AS Referentie,
      cast(Beschrijving AS nvarchar(120)) AS Beschrijving,
      Kwaliteitsniveau1, Kwaliteitsniveau2
      FROM Versie"
    )

  VersieId <- (Versie %>% summarise(Id = paste0(Id, collapse = ",")))$Id

  Criterium <-
    dbGetQuery(ConnectiePool, "SELECT Id, Naam FROM Criterium")

  Indicator <-
    dbGetQuery(ConnectiePool, "SELECT Id, CriteriumId, Naam FROM Indicator")

  Indicator_habitat <- #nolint: object_name_linter
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, IndicatorId,
        cast(Beschrijving AS nvarchar(1050)) AS Beschrijving,
        cast(Beschrijving_naSoorten AS nvarchar(200)) AS Beschrijving_naSoorten,
        cast(Maatregelen AS nvarchar(510)) AS Maatregelen,
        cast(Opmerkingen AS nvarchar(830)) AS Opmerkingen,
        cast(Referenties AS nvarchar(290)) AS Referenties,
        HabitattypeId, VersieId
        FROM Indicator_habitat
        WHERE HabitattypeId in (%s) and VersieId in (%s)",
        HabitattypeId, VersieId
      )
    )

  Indicator_habitatId <- #nolint: object_name_linter
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

  Indicator_beoordelingId <- #nolint: object_name_linter
    (IndicatortabellenKoppeling %>%
      summarise(Id = paste0(Indicator_beoordelingId, collapse = ","))
    )$Id

  Indicator_beoordeling <- #nolint: object_name_linter
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

  CombinerenVoorwaarden <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT Id, BeoordelingId, VoorwaardeID1, VoorwaardeID2,
        ChildID1, ChildID2, BewerkingOperator
        FROM CombinerenVoorwaarden
        WHERE BeoordelingId in (%s)",
        BeoordelingId
      )
    )   #BewerkingAND niet overgenomen, die mag eigenlijk ook weg in de brondb!

  VoorwaardeId <-
    paste0(
      unique(
        c(
          (
            CombinerenVoorwaarden %>% filter(!is.na(.data$VoorwaardeID1))
          )$VoorwaardeID1,
          (
            CombinerenVoorwaarden %>% filter(!is.na(.data$VoorwaardeID2))
          )$VoorwaardeID2
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
        StudiegroepId, SubAnalyseVariabeleId,
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
    ) %>%      #voorwaarden zonder onder- en bovengrens er voorlopig uit halen!
    filter(!is.na(.data$Ondergrens)) %>%
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

  IndicatorHabitatTaxonGroep <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT IndicatorHabitatId, TaxonGroepCode
        FROM IndicatorHabitatTaxonGroep
        WHERE IndicatorHabitatId in (%s)",
        Indicator_habitatId
      )
    )

  VoorwaardeTaxonGroep <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT VoorwaardeId, TaxonGroepCode
        FROM VoorwaardeTaxonGroep
        WHERE VoorwaardeId in (%s)",
        VoorwaardeId
      )
    )

  TaxonGroepCode <-
    paste0(
      unique(
        c(
          (VoorwaardeTaxonGroep %>%
             filter(!is.na(TaxonGroepCode)))$TaxonGroepCode,
          (IndicatorHabitatTaxonGroep %>%
             filter(!is.na(TaxonGroepCode)))$TaxonGroepCode
        )
      ),
      collapse = "','"
    )

  TaxonGroep <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT TaxonGroepCode,
        Omschrijving
        FROM Taxongroep
        WHERE TaxonGroepCode in ('%s')",
        TaxonGroepCode
      )
    )

  TaxonGroepTaxon <-
    dbGetQuery(
      ConnectiePool,
      sprintf(
        "SELECT TaxonGroepCode, TaxonKey
        FROM TaxonGroepTaxon
        WHERE TaxonGroepCode in ('%s')",
        TaxonGroepCode
      )
    )

  Taxon <-
    dbGetQuery(
      ConnectiePool,
      "SELECT scientificName as WetNaam,
      canonicalName,
      [Key] as GbifUsageKey,
      rank as Rank,
      kingdom AS Kingdom,
      genus AS Genus,
      species AS Species,
      kingdomKey AS KingdomKey,
      genusKey AS GenusKey,
      speciesKey AS SpeciesKey
      FROM Taxon"
    )

  QueryObservatieTaxon <-
    "SELECT TaxonName,
    TaxonKey AS GbifUsageKey,
    InboBronnen
  FROM ObservatieTaxon"
  ObservatieTaxon <- dbGetQuery(ConnectiePool, QueryObservatieTaxon)

  # voor tabel Taxon willen we ook de hogere taxa toevoegen
  # en aan tabel ObservatieTaxon willen we de keys van de synoniemen toevoegen
  # die halen we uit databank D0155_00_Taxa
  query <-
    "SELECT DISTINCT
      gbt.scientificNameExact AS ScientificNameExact,
      gbt.nubKey AS GbifUsageKey,
      gbt.rank AS Rank,
      gbt.kingdom AS Kingdom,
      gbt.phylum AS Phylum,
      gbt.[order] AS [Order],
      gbt.family AS Family,
      gbt.genus AS Genus,
      gbt.species AS Species,
      gbt.kingdomKey AS KingdomKey,
      gbt.phylumKey AS PhylumKey,
      gbt.classKey AS ClassKey,
      gbt.orderKey AS OrderKey,
      gbt.familyKey AS FamilyKey,
      gbt.genusKey AS GenusKey,
      gbt.speciesKey AS SpeciesKey,
      gbt.vernacularNameExact AS NLNameExact
    FROM TaxonSourceTaxonGbifMatch gm
      INNER JOIN GbifBackboneTaxon gbt ON gm.gbif_usageKey = gbt.nubKey
    WHERE gm.TaxonSourceName like 'LSVI - %'
      OR gm.TaxonSourceName like 'FLORA - %'"

  queryKeys <-
    "SELECT DISTINCT TaxonName, gbif_usageKey AS GbifKeyTaxonNaam
    FROM TaxonSourceTaxonGbifMatch
    WHERE gbif_usageKey is not null"

  con <- connect_inbo_dbase("D0155_00_Taxa")
  TaxonUitTaxa <- dbGetQuery(con, query)
  KeysUitTaxa <- dbGetQuery(con, queryKeys)
  dbDisconnect(con)

  TaxonAangevuld <- Taxon %>%
    left_join(
      TaxonUitTaxa %>%
        distinct(),
      by = "GbifUsageKey",
      suffix = c("", ".dbTaxa")
    ) %>%
    select(
      -"Rank.dbTaxa",
      -"Kingdom.dbTaxa",
      -"Genus.dbTaxa",
      -"Species.dbTaxa",
      -"KingdomKey.dbTaxa",
      -"GenusKey.dbTaxa",
      -"SpeciesKey.dbTaxa"
    )

  # Aan de tabel Observatietaxon voegen we NL namen en NbnTaxonVersionKeys
  # toe uit Florabank
  con <- connect_inbo_dbase("D0152_00_Flora")
  TaxonUitFlora <- dbGetQuery(
    con,
    "SELECT NaamNederlands, NaamWetenschappelijk,
      TaxonVersionKey AS NbnTaxonVersionKey
    FROM Taxon"
  )
  dbDisconnect(con)

  ObservatieTaxonAangevuld <- ObservatieTaxon %>%
    left_join(
      TaxonUitFlora %>%
        distinct(),
      by = c("TaxonName" = "NaamWetenschappelijk")
    ) %>%
    left_join(
      KeysUitTaxa,
      by = "TaxonName"
    ) %>%
    mutate(
      GbifKeyTaxonNaam = ifelse(
        is.na(.data$GbifKeyTaxonNaam),
        .data$GbifUsageKey,
        .data$GbifKeyTaxonNaam
      )
    )
  #aanpassing Gbif-namen!!!  Na definitieve migratie ook in brondb aanpassen?
  #(als het de gebruikers niet meer hindert als ze de kopie gebruiken)


  #berekening Theoretisch Maximum
  Voorwaarde <- Voorwaarde %>%
    left_join(
      VoorwaardeTaxonGroep %>%
        left_join(
          TaxonGroepTaxon,
          by = "TaxonGroepCode",
          relationship = "many-to-many"
        ) %>%
        distinct(.data$VoorwaardeId, .data$TaxonKey) %>%
        count(VoorwaardeId, name = "AantalSoorten"),
      by = c("Id" = "VoorwaardeId")
    ) %>%
    left_join(
      StudieItem,
      by = "StudiegroepId",
      suffix = c("", ".studie"),
      relationship = "many-to-many"
    ) %>%
    group_by(
      .data$Id, .data$VoorwaardeNaam, .data$ExtraInfo, .data$AnalyseVariabeleId,
      .data$Referentiewaarde, .data$Operator, .data$InvoermaskerId,
      .data$StudiegroepId, .data$SubAnalyseVariabeleId,
      .data$SubReferentiewaarde, .data$SubOperator, .data$SubInvoermaskerId,
      .data$AantalSoorten
    ) %>%
    summarise(AantalKenmerken = n()) %>%
    ungroup() %>%
    inner_join(AnalyseVariabele, by = c("AnalyseVariabeleId" = "Id")) %>%
    inner_join(TypeVariabele, by = c("TypeVariabeleId" = "Id")) %>%
    mutate(
      Maximumwaarde =
        ifelse(
          .data$VariabeleNaam %in%
            c("aandeel", "aandeelKruidlaag", "meting_perc"),
          1,
          NA
        ),
      Maximumwaarde =
        ifelse(
          grepl("bedekking", tolower(.data$VariabeleNaam)),
          1, .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", .data$VariabeleNaam) & .data$Naam == "Categorie",
          1, .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", .data$VariabeleNaam) & .data$Naam == "Ja/nee",
          1, .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", .data$VariabeleNaam) &
            .data$VoorwaardeNaam == "aantal geslachten",
          2, .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", .data$VariabeleNaam) &
            .data$VoorwaardeNaam == "bosconstantie",
          250, .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          .data$VariabeleNaam == "aantal",
          .data$AantalSoorten,
          .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          .data$VariabeleNaam == "aantal" & is.na(.data$Maximumwaarde) &
            !is.na(.data$StudiegroepId),
          .data$AantalKenmerken,
          .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          .data$VariabeleNaam == "aantalGroepen" & !is.na(.data$StudiegroepId),
          .data$AantalKenmerken,
          .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          grepl("meting", .data$VariabeleNaam) &
            .data$Naam != "Ja/nee" &
            !.data$VariabeleNaam %in% c("meting_perc", "meting_bedekking") |
            .data$VariabeleNaam == "aantal",
          pmin(3 * as.numeric(sub(",", ".", .data$Referentiewaarde)),
               .data$Maximumwaarde,
               na.rm = TRUE),
          .data$Maximumwaarde
        ),
      Maximumwaarde =
        ifelse(
          .data$VariabeleNaam == "scoresom",
          ifelse(.data$Referentiewaarde == 6, 15, 11),
          .data$Maximumwaarde
        )
    ) %>%
    select(
      "Id", "VoorwaardeNaam", "ExtraInfo", "AnalyseVariabeleId",
      "Referentiewaarde", "Operator", "InvoermaskerId",
      "StudiegroepId", "SubAnalyseVariabeleId", "SubReferentiewaarde",
      "SubOperator", "SubInvoermaskerId", "Maximumwaarde"
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
  dbWriteTable(NieuweDb, "IndicatortabellenKoppeling",
               IndicatortabellenKoppeling)
  dbWriteTable(NieuweDb, "Lijst", Lijst)
  dbWriteTable(NieuweDb, "LijstItem", LijstItem)
  dbWriteTable(NieuweDb, "Studiegroep", Studiegroep)
  dbWriteTable(NieuweDb, "StudieItem", StudieItem)
  dbWriteTable(NieuweDb, "Taxon", TaxonAangevuld)
  dbWriteTable(NieuweDb, "ObservatieTaxon", ObservatieTaxonAangevuld)
  dbWriteTable(NieuweDb, "TaxonGroep", TaxonGroep)
  dbWriteTable(
    NieuweDb,
    "IndicatorHabitatTaxonGroep",
    IndicatorHabitatTaxonGroep
  )
  dbWriteTable(NieuweDb, "VoorwaardeTaxonGroep", VoorwaardeTaxonGroep)
  dbWriteTable(NieuweDb, "TaxonGroepTaxon", TaxonGroepTaxon)
  dbWriteTable(NieuweDb, "TypeVariabele", TypeVariabele)
  dbWriteTable(NieuweDb, "Versie", Versie)
  dbWriteTable(NieuweDb, "Voorwaarde", Voorwaarde)
  dbExecute(NieuweDb, "CREATE INDEX idx_tguk ON Taxon(GbifUsageKey)")
  dbExecute(NieuweDb, "CREATE INDEX idx_otguk ON ObservatieTaxon(GbifUsageKey)")
  dbExecute(NieuweDb, "CREATE INDEX idx_ottn ON ObservatieTaxon(TaxonName)")
  dbExecute(
    NieuweDb,
    "CREATE INDEX idx_otnn ON ObservatieTaxon(NaamNederlands)"
  )
  dbDisconnect(NieuweDb)
}

#databank wissen
unlink("inst/databank/LSVIHabitatTypes.sqlite")

#databank terug vullen
LSVI:::maakConnectiePoolServer()
migratieSQLserverSQLite()
