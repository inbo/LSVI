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
berekenLSVIbasis(Versie = "alle", Kwaliteitsniveau = "1", Data_voorwaarden)

#onderstaande is gekopieerd en moet nog aangepast worden aan de te testen funtie!!!

# test_that("parameter versie heeft correct formaat", {
#   expect_equal(berekenLSVIbasis(Versie = "alle", 
#                            Kwaliteitsniveau = "alle", 
#                            Data_voorwaarden),
#                "Invoer OK")
#   expect_equal(berekenLSVI(Versie = "Versie 3", 
#                            Kwaliteitsniveau = "alle", 
#                            Data_indicatoren),
#                "Invoer OK")
#   expect_error(berekenLSVI(Versie = 2,
#                            Kwaliteitsniveau = "alle",
#                            Data_indicatoren),
#                "Error in match.arg\\(Versie\\) : 'arg' must be NULL or a character vector\n")
#   expect_that(berekenLSVI(Versie = 2, 
#                           Kwaliteitsniveau = "alle", 
#                           Data_indicatoren),
#               throws_error())
# })
# 
# test_that("parameter kwaliteitsniveau heeft correct formaat", {
#   expect_equal(berekenLSVI(Versie = "alle", 
#                            Kwaliteitsniveau = "1", 
#                            Data_indicatoren),
#                "Invoer OK")
#   expect_equal(berekenLSVI(Versie = "alle", 
#                            Kwaliteitsniveau = 1, 
#                            Data_indicatoren),
#                "Invoer OK")
#   expect_error(berekenLSVI(Versie = "alle",
#                            Kwaliteitsniveau = "streefwaarde",
#                            Data_indicatoren),
#                "Error in match.arg\\(Kwaliteitsniveau\\) : \n  'arg' should be one of *")
# })
# 
# test_that("dataframe Data_indicatoren heeft correct formaat", {
#   expect_equal(berekenLSVI(Versie = "alle", 
#                            Kwaliteitsniveau = "alle", 
#                            Data_indicatoren),
#                "Invoer OK")
# })
