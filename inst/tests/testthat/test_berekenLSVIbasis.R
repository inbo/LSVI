context("test berekenLSVIbasis")

library(readr)
library(dplyr)


Schaalomzetting <- read_csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"))
Aantal_soorten_frequent <-
      data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
       VoorwaardeID = c(2, 1, 2, 1, 2, 1),
       Waarde = c("3","1","0","0","1","0"),
       Habitatsubtype = 4010,
       stringsAsFactors = FALSE)
Data_voorwaarden <- read_csv2(system.file("vbdata/opname_4010_gelayout_indicatoren.csv", package = "LSVI")) %>%
  left_join(Schaalomzetting, by = c("Waarde" = "Schaal_opname")) %>%
  mutate_(
    Waarde = ~ifelse(!is.na(Percentage), Percentage,
                     ifelse(!is.na(Tansley), Tansley, Waarde))
  ) %>%
  left_join(data.frame(Indicator = c("dwergstruiken", "veenmoslaag", "vergrassing", "verbossing"),
                       VoorwaardeID = c(3,4,5,7),
                       stringsAsFactors = FALSE),
            by = c("Indicator" = "Indicator")) %>%
  select_(~ID, ~VoorwaardeID, ~Waarde, ~Habitatsubtype) %>%
  bind_rows(Aantal_soorten_frequent)

# Resultaat <- berekenLSVIbasis(Versie = "alle", Kwaliteitsniveau = "alle", Data_voorwaarden)
# save(Resultaat, file = "inst/vbdata/Resultaat_test.Rdata")
# load("inst/vbdata/Resultaat_test.Rdata")
load(system.file("vbdata/Resultaat_test.Rdata", package = "LSVI"))
Resultaat_versie3 <- list(Resultaat[[1]] %>% filter_(~VersieLSVI == "Versie 3"),
                          Resultaat[[2]] %>% filter_(~VersieLSVI == "Versie 3"),
                          Resultaat[[3]] %>% filter_(~VersieLSVI == "Versie 3"))
Resultaat_kwal1 <- list(Resultaat[[1]] %>% filter_(~Kwaliteitsniveau == 1),
                          Resultaat[[2]] %>% filter_(~Kwaliteitsniveau == 1),
                          Resultaat[[3]] %>% filter_(~Kwaliteitsniveau == 1))

test_that("ConnectieLSVIhabitats is een open RODBC-connectie", {
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats = "geenConnectie", Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden),
               "ConnectieLSVIhabitats does not inherit from class RODBC")
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden), 
               Resultaat)
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})


test_that("parameter versie heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden), 
               Resultaat)
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "Versie 3",
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               Resultaat_versie3)
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = 2,
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               "Versie is not a string")
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("parameter kwaliteitsniveau heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = "1",
                           Data_voorwaarden),
               Resultaat_kwal1)
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = 1,
                           Data_voorwaarden),
               Resultaat_kwal1)
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = "streefwaarde",
                           Data_voorwaarden),
               "Kwaliteitsniveau moet een van de volgende waarden zijn")
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("dataframe Data_voorwaarden heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               Resultaat)
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == 1, "één", Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: geen getal ingevoerd waar een getal verwacht wordt")
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "1", "-1", Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: een negatief getal ingevoerd")
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "1", "11.2", Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: een kommagetal ingevoerd waar een geheel getal verwacht wordt")
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "1", NA, Waarde)
                                  )),
               list(Resultaat[[1]] %>%
                      mutate_(
                        Beoordeling_criterium =
                          ~ifelse(Criterium == "Vegetatie" & (ID %in% c("Jo1380", "WT0174")),
                                  NA, Beoordeling_criterium)
                      ),
                    Resultaat[[2]] %>%
                      mutate_(
                        Beoordeling_indicator =
                          ~ifelse(Criterium == "Vegetatie" & (ID %in% c("Jo1380", "WT0174")),
                                  NA, Beoordeling_indicator)
                      ),
                    Resultaat[[3]] %>%
                      mutate_(
                        Waarde = ~ifelse(Waarde == "1", NA, Waarde),
                        Status = ~ifelse(Waarde == "1", NA, Status),
                        Beoordeling_indicator =
                          ~ifelse(Criterium == "Vegetatie" & (ID %in% c("Jo1380", "WT0174")),
                                  NA, Beoordeling_indicator)
                      )))
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == 75, "zeven", Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: geen getal ingevoerd waar een getal verwacht wordt")
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == 75, -75, Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: een negatief getal ingevoerd")
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == 75, 175, Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: een getal > 100 ingevoerd waar een percentage verwacht wordt")
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == 75, NA, Waarde)
                                  )),
               list(Resultaat[[1]] %>%
                      mutate_(
                        Beoordeling_criterium =
                          ~ifelse(Criterium == "Verstoring" & (ID %in% c("WT0173")),
                                  NA, Beoordeling_criterium)
                      ),
                    Resultaat[[2]] %>%
                      mutate_(
                        Beoordeling_indicator =
                          ~ifelse(Indicator == "vergrassing" & (ID %in% c("WT0173")),
                                  NA, Beoordeling_indicator)
                      ),
                    Resultaat[[3]] %>%
                      mutate_(
                        Waarde = ~ifelse(Waarde == 75, NA, Waarde),
                        Status = ~ifelse(Waarde == 75, NA, Status),
                        Beoordeling_indicator =
                          ~ifelse(Indicator == "vergrassing" & (ID %in% c("WT0173")),
                                  NA, Beoordeling_indicator)
                      )))
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "zeldzaam",
                                                     "foute invoer", Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: niet alle categorische waarden komen overeen met het invoermasker uit de databank")
  expect_error(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "zeldzaam", 2, Waarde)
                                  )),
               "Foute invoer in Data_voorwaarden\\$Waarde: niet alle categorische waarden komen overeen met het invoermasker uit de databank")
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "zeldzaam", "ZELDZAAM", Waarde)
                                  )),
               list(Resultaat[[1]], Resultaat[[2]],
                    Resultaat[[3]] %>%
                      mutate_(
                        Waarde = ~ifelse(Waarde == "zeldzaam", "ZELDZAAM", Waarde)
                      )))
  expect_equal(berekenLSVIbasis(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_voorwaarden %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "zeldzaam", NA, Waarde)
                                  )),
               list(Resultaat[[1]] %>%
                      mutate_(
                        Beoordeling_criterium =
                          ~ifelse(Criterium == "Structuur" & (ID %in% c("WT0173")),
                                  NA, Beoordeling_criterium)
                      ),
                    Resultaat[[2]] %>%
                      mutate_(
                        Beoordeling_indicator =
                          ~ifelse(Indicator == "veenmoslaag" & (ID %in% c("WT0173")),
                                  NA, Beoordeling_indicator)
                      ),
                    Resultaat[[3]] %>%
                      mutate_(
                        Waarde = ~ifelse(Waarde == "zeldzaam", NA, Waarde),
                        Status = ~ifelse(Waarde == "zeldzaam", NA, Status),
                        Beoordeling_indicator =
                          ~ifelse(Indicator == "veenmoslaag" & (ID %in% c("WT0173")),
                                  NA, Beoordeling_indicator)
                      )))
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

#werking childID nog testen!



