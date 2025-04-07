# Dit script zorgt voor het migreren van een Taxontabel uit SQL-server
# naar een csv-file in het package zelf in de map inst/databank

library(DBI)
library(inbodb)
library(dplyr)
library(readr)

query <-
  "SELECT DISTINCT gm.TaxonNameExact,
    gbt.scientificNameExact AS ScientificNameExact,
    gm.gbif_usageKey AS GbifUsageKey,
    gm.gbif_confidence AS GbifConfidence,
    gm.gbif_matchType AS GbifMatchType,
    gm.gbif_acceptedusageKey AS GbifAcceptedUsageKey,
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

# bij dubbele namen die met de hoogste betrouwbaarheid nemen
# (dubbels met afwijkende conf omdat bij LSVI het kingdom niet toegevoegd is)
taxontabel <- taxontabel %>%
  distinct() %>%
  mutate(
    Conf = GbifConfidence - 20 * (GbifMatchType == "NONE") -
      10 * (GbifMatchType == "FUZZY")
  ) %>%
  group_by(TaxonNameExact) %>%
  mutate(
    N = n(),
    Maxconf = max(Conf)
  ) %>%
  ungroup() %>%
  filter(Conf == Maxconf) %>%
  left_join(
    taxontabel %>%
      select("TaxonNameExact", NbnKey = "NbnTaxonVersionKey") %>%
      filter(!is.na(NbnKey)) %>%
      distinct(),
    by = "TaxonNameExact"
  ) %>%
  mutate(
    NbnTaxonVersionKey =
      ifelse(is.na(NbnTaxonVersionKey), NbnKey, NbnTaxonVersionKey)
  ) %>%
  distinct() %>%
  select(-N, -Conf, -Maxconf, -NbnKey) %>%
  arrange(TaxonNameExact)

write_csv2(
  taxontabel,
  file = "inst/databank/TaxonTabel.csv"
)
