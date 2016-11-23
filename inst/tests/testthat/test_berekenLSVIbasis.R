context("test berekenLSVIbasis")

library(dplyr)

Schaalomzetting <- read.csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"), stringsAsFactors = FALSE)
Aantal_soorten_frequent <- 
      data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
       VoorwaardeID = c(2, 1, 2, 1, 2, 1),
       Waarde = c("3","1","0","0","1","0"),
       Habitatsubtype = 4010,
       stringsAsFactors = FALSE)
Data_voorwaarden <- read.csv2(system.file("vbdata/opname_4010_gelayout_indicatoren.csv", package = "LSVI"), stringsAsFactors = FALSE) %>%
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


#onderstaande is gekopieerd en moet nog aangepast worden aan de te testen functie!!!

test_that("parameter versie heeft correct formaat", {
  expect_equal(berekenLSVIbasis(Versie = "alle",
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               Resultaat)
  expect_equal(berekenLSVIbasis(Versie = "Versie 3",
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               Resultaat_versie3)
  expect_error(berekenLSVIbasis(Versie = 2,
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               "Error in match.arg\\(Versie\\) : 'arg' must be NULL or a character vector\n")
})
 
test_that("parameter kwaliteitsniveau heeft correct formaat", {
  expect_equal(berekenLSVIbasis(Versie = "alle",
                           Kwaliteitsniveau = "1",
                           Data_voorwaarden),
               Resultaat_kwal1)
  expect_equal(berekenLSVIbasis(Versie = "alle",
                           Kwaliteitsniveau = 1,
                           Data_voorwaarden),
               Resultaat_kwal1)
  expect_error(berekenLSVIbasis(Versie = "alle",
                           Kwaliteitsniveau = "streefwaarde",
                           Data_voorwaarden),
               "Error in match.arg\\(Kwaliteitsniveau\\) : \n  'arg' should be one of *")
})

test_that("dataframe Data_voorwaarden heeft correct formaat", {
  expect_equal(berekenLSVIbasis(Versie = "alle",
                           Kwaliteitsniveau = "alle",
                           Data_voorwaarden),
               Resultaat)
  # expect_equal(berekenLSVIbasis(Versie = "alle",
  #                               Kwaliteitsniveau = "alle",
  #                               Data_voorwaarden %>% 
  #                                 mutate_(
  #                                   Waarde = ~ifelse(Waarde == 1, -1, Waarde)  #zou foutmelding moeten geven
  #                                 )),
  #              Resultaat)
  # expect_equal(berekenLSVIbasis(Versie = "alle",
  #                               Kwaliteitsniveau = "alle",
  #                               Data_voorwaarden %>% 
  #                                 mutate_(
  #                                   Waarde = ~ifelse(Waarde == 1, 11.1, Waarde)  #zou foutmelding moeten geven
  #                                 )),
  #              Resultaat)
  # expect_equal(berekenLSVIbasis(Versie = "alle",
  #                               Kwaliteitsniveau = "alle",
  #                               Data_voorwaarden %>% 
  #                                 mutate_(
  #                                   Waarde = ~ifelse(Waarde == 75, -75, Waarde)  #zou foutmelding moeten geven
  #                                 )),
  #              Resultaat)
  # expect_equal(berekenLSVIbasis(Versie = "alle",
  #                               Kwaliteitsniveau = "alle",
  #                               Data_voorwaarden %>%
  #                                 mutate_(
  #                                   Waarde = ~ifelse(Waarde == "zeldzaam", 
  #                                                    "foute invoer", Waarde)  #zou foutmelding moeten geven
  #                                 )),
  #              Resultaat)
})
