# Dit script zorgt voor het migreren van een Taxontabel uit SQL-server
# naar een csv-file in het package zelf in de map inst/databank

library(DBI)
library(inbodb)
library(dplyr)
library(readr)
library(stringr)

query <-
  "SELECT DISTINCT --gm.TaxonNameExact,
    gbt.scientificNameExact AS ScientificNameExact,
    gbt.nubKey AS GbifUsageKey,
    --gm.gbif_confidence AS GbifConfidence,
    --gm.gbif_matchType AS GbifMatchType,
    --gm.gbif_acceptedusageKey AS GbifAcceptedUsageKey,
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
    json_value(gm.Keys, '$[0].NbnTaxonVersionKey') AS NbnTaxonVersionKey,
    gbt.vernacularNameExact AS NLNameExact
  FROM TaxonSourceTaxonGbifMatch gm
    INNER JOIN GbifBackboneTaxon gbt ON gm.gbif_usageKey = gbt.nubKey
  WHERE gm.TaxonSourceName like 'LSVI - %'
    OR gm.TaxonSourceName like 'FLORA - %'"

con <- connect_inbo_dbase("D0155_00_Taxa")
taxontabel <- dbGetQuery(con, query)
dbDisconnect(con)

QueryTaxon <-
  "SELECT scientificName AS TaxonNameExact,
    scientificName AS ScientificNameExact,
    nubKey,
    [Key] AS GbifUsageKey,
    --gm.gbif_confidence AS GbifConfidence,
    --gm.gbif_matchType AS GbifMatchType,
    acceptedKey AS GbifAcceptedUsageKey,
    rank AS Rank,
    kingdom AS Kingdom,
    --gbt.phylum AS Phylum,
    --gbt.[order] AS [Order],
    --gbt.family AS Family,
    genus AS Genus,
    species AS Species,
    kingdomKey AS KingdomKey,
    --gbt.phylumKey AS PhylumKey,
    --gbt.classKey AS ClassKey,
    --gbt.orderKey AS OrderKey,
    --gbt.familyKey AS FamilyKey,
    genusKey AS GenusKey,
    speciesKey AS SpeciesKey,
    --json_value(gm.Keys, '$[0].NbnTaxonVersionKey') AS NbnTaxonVersionKey,
    vernacularName AS NLNameExact,
    canonicalName, --extra, voorlopig niet gebruikt
    authorship, --extra, voorlopig niet gebruikt
    taxonomicStatus, -- ACCEPTED, SYNONYM, DOUBTFUL, HETEROTYPIC_SYNONYM, HOMOTYPIC_SYNONYM, PROPARTE_SYNONYM
    nameType --SCIENTIFIC or HYBRID
  FROM Taxon"

LSVI:::maakConnectiePoolServer(Databank = "D0122_02_LsviHabitattypes")
Taxon <- dbGetQuery(ConnectiePool, QueryTaxon)

# Taxon <-
#   dbGetQuery(
#     ConnectiePool,
#     "SELECT 0 as NbnTaxonVersionKey, scientificName as FloraNaamWetenschappelijk,
#       vernacularName as FloraNaamNederlands,
#       [Key] as GbifUsageKey,
#       acceptedKey as GbifAcceptedUsageKey,
#       rank as Rank --,
#       --GbifUsageKeyFloraNaamWetenschappelijk
#       FROM Taxon"
#   )

TaxontabelNieuw <- Taxon %>% #van script migratieSQLserverSQLite.R
  left_join(
    taxontabel %>%
      distinct() %>%
      left_join(
        taxontabel %>%
          select("ScientificNameExact", NbnKey = "NbnTaxonVersionKey") %>%
          filter(!is.na(NbnKey)) %>%
          distinct(),
        by = "ScientificNameExact"
      ) %>%
      mutate(
        NbnTaxonVersionKey =
          ifelse(is.na(NbnTaxonVersionKey), NbnKey, NbnTaxonVersionKey)
      ) %>%
      select(-"NbnKey") %>%
      distinct(),
    by = "GbifUsageKey",
    suffix = c("", ".dbTaxa")
  ) %>%
  select(
    -"ScientificNameExact.dbTaxa", -"Rank.dbTaxa",
    -"Kingdom.dbTaxa", -"Genus.dbTaxa", -"Species.dbTaxa",
    -"KingdomKey.dbTaxa", -"GenusKey.dbTaxa", -"SpeciesKey.dbTaxa",
    -"NLNameExact.dbTaxa"
  )
# opmerking: er zijn wel enkele records dubbel omdat sommige soorten 2 NbnTaxonVersionKeys hebben

QueryObservatieTaxon <-
  "SELECT TaxonName,
    TaxonKey AS GbifUsageKey,
    InboBronnen
  FROM ObservatieTaxon"
ObservatieTaxon <- dbGetQuery(ConnectiePool, QueryObservatieTaxon)
ObservatieTaxonExtra <- ObservatieTaxon %>%
  left_join(
    taxontabel %>%
      distinct(ScientificNameExact, GbifUsageKey, NbnTaxonVersionKey) %>%
      left_join(
        taxontabel %>%
          select("ScientificNameExact", NbnKey = "NbnTaxonVersionKey") %>%
          filter(!is.na(NbnKey)) %>%
          distinct(),
        by = "ScientificNameExact"
      ) %>%
      mutate(
        NbnTaxonVersionKey =
          ifelse(is.na(NbnTaxonVersionKey), NbnKey, NbnTaxonVersionKey)
      ) %>%
      select(-"NbnKey") %>%
      distinct(),
    by = "GbifUsageKey",
    suffix = c("", ".dbTaxa")
  )

  #en daarna in de rest van het package zoeken waar verwijderde velden uit migratieSQLserverSQLite naartoe zijn


# Bijwerken op basis van tabellen in LSVI
# con <- LSVI:::connecteerMetLSVIdbServer()
# TaxonlijstNieuw2 <- DBI::dbGetQuery(con, "select * from nieuw2.Taxon")
# SynoniemenlijstNieuw2 <-
#   DBI::dbGetQuery(con, "select * from nieuw2.TaxonSynoniem")
# DBI::dbDisconnect(con)
#
# test <- taxontabel %>%
#   left_join(
#     SynoniemenlijstNieuw2 %>%
#       filter(
#         Taal == "la",
#         !(Synoniem == "Eragrostis virescens subsp. verloovei Portal"
#           & Bron == "self"),
#         !(Synoniem == "Rosa dumetorum Thuill."
#           & Bron == "flora synonym wetenschappelijk")
#       ) %>%
#       transmute(  #Er zijn nog een 120-tal gelijkaardige: ofwel niet in synoniemenlijst, ofwel verschil in schrijfwijze
#         Synoniem =
#           ifelse(Synoniem == "Absconditella fossarum Vezda & Pisút",
#                  "Absconditella fossarum Vězda & Pisút", Synoniem),
#         AcceptedKeyFlora = nubkey
#       ) %>%
#       distinct(),
#     by = c("TaxonNameExact" = "Synoniem")
#   ) %>%
#   left_join(
#     SynoniemenlijstNieuw2 %>%
#       filter(Taal == "nl") %>%
#       transmute(
#         Synoniem,
#         AcceptedKeyFloraNL = nubkey
#       ) %>%
#       distinct(),
#     by = c("NLNameExact" = "Synoniem")
#   ) %>%
#   left_join(
#     TaxonlijstNieuw2,
#     by =
#   )


# # bij dubbele namen die met de hoogste betrouwbaarheid nemen
# # (dubbels met afwijkende conf omdat bij LSVI het kingdom niet toegevoegd is)
# taxontabel <- taxontabel %>%
#   distinct() %>%
#   mutate(
#     Conf = GbifConfidence - 20 * (GbifMatchType == "NONE") -
#       10 * (GbifMatchType == "FUZZY")
#   ) %>%
#   group_by(TaxonNameExact) %>%
#   mutate(
#     N = n(),
#     Maxconf = max(Conf)
#   ) %>%
#   ungroup() %>%
#   filter(Conf == Maxconf) %>%
#   left_join(
#     taxontabel %>%
#       select("TaxonNameExact", NbnKey = "NbnTaxonVersionKey") %>%
#       filter(!is.na(NbnKey)) %>%
#       distinct(),
#     by = "TaxonNameExact"
#   ) %>%
#   mutate(
#     NbnTaxonVersionKey =
#       ifelse(is.na(NbnTaxonVersionKey), NbnKey, NbnTaxonVersionKey),
#     TaxonNameExact = gsub("\\s", " ", TaxonNameExact),
#     TaxonNameExact = gsub("\U00A0", " ", TaxonNameExact)
#   ) %>%
#   distinct() %>%
#   select(-N, -Conf, -Maxconf, -NbnKey) %>%
#   arrange(TaxonNameExact)
#
# # Enkele tijdelijke aanpassingen om de bron op te kuisen
# taxontabel <- taxontabel %>%
#   mutate(
#     TaxonNameExactOrig = TaxonNameExact,
#     NLNameExact = tolower(NLNameExact),
#     TaxonNameExact = gsub("(.*)  (.*)", "\\1 \\2", TaxonNameExact),
#     TaxonNameExact =
#       ifelse(
#         Rank == "GENUS" & str_count(TaxonNameExact, " ") == 0,
#         ScientificNameExact,
#         TaxonNameExact
#       ),
#     TaxonNameExact =
#       ifelse(
#         Rank == "SPECIES" & str_count(TaxonNameExact, " ") == 1,
#         ScientificNameExact,
#         TaxonNameExact
#       ),
#     TaxonNameExact = gsub("(.*) x (.*)", "\\1 ×\\2", TaxonNameExact),
#     TaxonNameExact = gsub("(.*)× (.*)", "\\1×\\2", TaxonNameExact),
#     TaxonNameExact =
#       gsub("(.*) L\\. var\\. (.*)", "\\1 var. \\2", TaxonNameExact),
#     TaxonNameExact =
#       gsub("(.*) L\\. subsp\\. (.*)", "\\1 subsp. \\2", TaxonNameExact),
#     TaxonNameExact = gsub("(.*) \\: Fr\\.(.*)", "\\1\\2", TaxonNameExact),
#     TaxonNameExact =
#       gsub("(.*) Nolte Nolte (.*)", "(.*) Nolte (.*)", TaxonNameExact),
#     TaxonNameExact =
#       gsub("Wilcz\\.\\&", "Wilcz. &", TaxonNameExact),
#     TaxonNameExact =
#       ifelse(
#         TaxonNameExact == "Fissidens dubius P.Beauv.var. dubius",
#         "Fissidens dubius P.Beauv. var. dubius",
#         TaxonNameExact
#       )
#   ) %>%
#   filter(
#     GbifMatchType == "EXACT",
#     !grepl("nom. rejec.", TaxonNameExact),
#     !(TaxonNameExact == "Rosa ×nitidula Besser" & is.na(NbnTaxonVersionKey)),
#     !TaxonNameExact %in%
#       c(
#         "Aethusa cynapium subsp. cynapioides auct. an (Bieb.) Nyman?",
#         "Agrimonia odorata auct. non Mill.",
#         "Amsinckia menziesii auct. non (Lehm. ex Fisch. et C.A. Mey.) A. Nelson et Macbr.",
#         "Anagallis arvensis subsp. arvensis f. carnea",
#         "Arenaria serpyllifolia subsp. serpyllifolia var. lloydii (Jord.) Lloyd",
#         "Arenaria serpyllifolia subsp. serpyllifolia var. serpyllifolia",
#         "Arenaria serpyllifolia subsp. serpyllifolia var. viscida (Haller f.) DC.",
#         "Arrhenatherum elatius (L.) Beauv. ex J. et C. Presl subsp. bulbosum (Willd.) Schübl. et Martens",
#         "Aster tradescantii auct. an L.?",
#         "Calamintha vulgaris (L.) Halácsy non Clairv.",
#         "Carex leporina auct. non var. argyroglochin (Hornem.) Koch",
#         "Carex viridula Michaux subsp. brachyrrhyncha (Čelak.) B. Schmid var. elatior (Schlecht.) Crins",
#         "Carex viridula Michaux subsp. brachyrrhyncha (Čelak.) B. Schmid var. lepidocarpa (Tausch) B. Schmid",
#         "Carex flava L. sensu lato",
#         "Chrysanthemum balsamita (L.) Baillon non L.",
#         "Chrysanthemum tanacetum Karsch non Vis.",
#         "Collema tenax (Sw.) Ach. em. Degel.", "Nitella flexilis/opaca",
#         "Fallopia japonica (Houtt.) Ronse Decraene var. compacta (Hook f.) J. Bailey",
#         "Filago germanica L. non Huds. var. lutescens (Jord.) Gren. et Godr.",
#         "Melanelia glabratula (Lamy) Essl. ssp.glabratula",
#         "Ononis repens + spinosa",
#         "Ornithogalum umbellatum sensu v. Raamsdonk",
#         "Phleum nodosum auct. non L.",
#         "Pronectria oligospora Lowen & Rogerson var. Octospora",
#         "Ranunculus L. subg. Batrachium",
#         "Rosa spinosissima auct.",
#         "Rubus montanus auct. non Libert ex Lej. nec Wirtg.",
#         "Salicornia herbacea L. subvar. brachystachya G.F.W. Mey.",
#         "Salix babylonica var. pekinensis A. Henry cv. Tortuosa",
#         "Salix sachalinensis F.Schmidt var. Sekka",
#         "Senecio bicolor (Willd.) Tod. non Viv. subsp. cineraria (DC.) Chater",
#         "Thlaspi alpestre L. non Jacq. subsp. calaminare (Lej.) O. Schwarz",
#         "Thlaspi alpestre L. non Jacq. var. calaminare Lej."
#       )
#   ) %>%
#   distinct() %>%
#   mutate(
#     NLNameExact =
#       ifelse(TaxonNameExact == "Elytrigia juncea (l.) Nevski", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Palustriella Ochyra", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Halimione pedunculata (L.) Aell.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Halimione portulacoides (L.) Aell.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "hauwmos p.p.", NA, NLNameExact), #NL namen van hauwmos doen we weg wegens dubieus (= naam voor volledig phylum)
#     NLNameExact =
#       ifelse(TaxonNameExact == "Conocephalum Hill", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "kronkelbladmos p.p.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Chiloscyphus Corda", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "pronkmos p.p.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "schorpioenmos p.p.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "schotelkorst", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "schriftmos", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "sikkelmos p.p.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(NLNameExact == "snavelmos p.p.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Persicaria lapathifolia (L.) S.F. Gray",
#              NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Schedonorus pratensis (Huds.) P.Beauv.",
#              NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Sesleria albicans Kit. ex Schult.",
#              NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Carex pairae F.W. Schultz", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Rinodina gennarii Bagl.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Stellaria pallida (Dum.) Piré", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Lysimachia minima (L.) U. Manns & Anderb.",
#              "dwergbloem", NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Parmotrema stuppeum (Taylor) Hale",
#              NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Bryum bicolor Dicks.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Eurhynchium pumilum (Wils.) Schimp.",
#              NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Aster lanceolatus Willd.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Caloplaca lithophila H. Magn.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Verrucaria glaucina Ach.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Bryum imbricatum (Schwägr.) Bruch & Schimp.",
#              NA, NLNameExact),
#     NLNameExact =
#       ifelse(grepl("Schedonorus", TaxonNameExact), NA, NLNameExact),
#     NLNameExact =
#       ifelse(
#         TaxonNameExact == "Punctelia ulophylla (Ach.) van Herk & Aptroot",
#         NA, NLNameExact
#       ),
#     NLNameExact =
#       ifelse(
#         TaxonNameExact == "Erysimum hieraciifolium auct. non L.",
#         NA, NLNameExact
#       ),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Sedum reflexum L.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Scutellaria altissima L.", NA, NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Mentha suaveolens Ehrh.",
#              "witte munt", NLNameExact),
#     NLNameExact =
#       ifelse(TaxonNameExact == "Carex curta Good.", NA, NLNameExact)
#   )
#
# taxontabel <- taxontabel %>%
#   left_join(
#     taxontabel %>%
#       select(-"TaxonNameExact", -"GbifConfidence", -"GbifMatchType", -"NbnTaxonVersionKey") %>%
#       distinct() %>%
#       count(NLNameExact) %>%
#       filter(!is.na(NLNameExact), n > 1),
#     by = "NLNameExact"
#   ) %>%
#   mutate(
#     RemoveNLName = n > 1 & !is.na(GbifAcceptedUsageKey)
#   ) %>%
#   group_by(NLNameExact) %>%
#   mutate(
#     AllTrue = sum(RemoveNLName) > n - 1
#   ) %>%
#   ungroup() %>%
#   mutate(
#     RemoveNLName = ifelse(AllTrue, FALSE, RemoveNLName),
#     NLNameExact = ifelse(!is.na(RemoveNLName) & RemoveNLName, NA, NLNameExact)
#   ) %>%
#   select(-"n", -"RemoveNLName", -"AllTrue")
#
# taxontabel <- taxontabel %>%
#   left_join(
#     taxontabel %>%
#       select(-"TaxonNameExact", -"GbifConfidence", -"GbifMatchType", -"NbnTaxonVersionKey") %>%
#       distinct() %>%
#       count(NLNameExact) %>%
#       filter(!is.na(NLNameExact), n > 1),
#     by = "NLNameExact"
#   ) %>%
#   mutate(
#     RemoveNLName = n > 1 & Rank == "SUBSPECIES"
#   ) %>%
#   group_by(NLNameExact) %>%
#   mutate(
#     AllTrue = sum(RemoveNLName) > n - 1
#   ) %>%
#   ungroup() %>%
#   mutate(
#     RemoveNLName = ifelse(AllTrue, FALSE, RemoveNLName),
#     NLNameExact = ifelse(!is.na(RemoveNLName) & RemoveNLName, NA, NLNameExact)
#   ) %>%
#   select(-"n", -"RemoveNLName", -"AllTrue")
#
# taxontabel <- taxontabel %>%
#   left_join(
#     taxontabel %>%
#       select(-"TaxonNameExact", -"GbifConfidence", -"GbifMatchType", -"NbnTaxonVersionKey") %>%
#       distinct() %>%
#       count(NLNameExact) %>%
#       filter(!is.na(NLNameExact), n > 1),
#     by = "NLNameExact"
#   ) %>%
#   mutate(
#     RemoveNLName = n > 1 & Rank == "SPECIES"
#   ) %>%
#   group_by(NLNameExact) %>%
#   mutate(
#     AllTrue = sum(RemoveNLName) > n - 1
#   ) %>%
#   ungroup() %>%
#   mutate(
#     RemoveNLName = ifelse(AllTrue, FALSE, RemoveNLName),
#     NLNameExact = ifelse(!is.na(RemoveNLName) & RemoveNLName, NA, NLNameExact)
#   ) %>%
#   select(-"n", -"RemoveNLName", -"AllTrue")

write_csv2(
  TaxontabelNieuw,
  file = "inst/databank/TaxonTabel.csv"
)
write_csv2(
  ObservatieTaxonExtra,
  file = "inst/databank/ObservatieTaxon.csv"
)
