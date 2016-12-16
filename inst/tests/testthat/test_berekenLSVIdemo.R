context("test berekenLSVIdemo")

library(readr)
library(dplyr)


Data_indicatoren <-
  read_csv2(system.file("vbdata/opname_4010_gelayout_indicatoren.csv", package = "LSVI"))
Data_soorten <-
  read_csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"))

load(system.file("vbdata/Resultaat_test.Rdata", package = "LSVI"))
Resultaat <- list(Resultaat[[1]] %>% filter_(~!is.na(ID)),
                  Resultaat[[2]] %>% filter_(~!is.na(ID)),
                  Resultaat[[3]] %>% filter_(~!is.na(ID)) %>%
                    select_(~ID, ~VoorwaardeID, ~Beoordeling_indicator, ~BeoordelingID,
                            ~Habitatsubtype, ~Waarde, ~VoorwaardeNaam, ~Referentiewaarde,
                            ~Operator, ~SoortengroepID, ~SoortengroepNaam, ~VariabeleNaam,
                            ~Eenheid, ~TypeVariabele, ~Vegetatielaag, ~Status,
                            ~VersieLSVI, ~Criterium, ~Indicator, ~Kwaliteitsniveau,
                            ~Beoordeling_letterlijk))
Resultaat_versie3 <- list(Resultaat[[1]] %>% filter_(~VersieLSVI == "Versie 3"),
                          Resultaat[[2]] %>% filter_(~VersieLSVI == "Versie 3"),
                          Resultaat[[3]] %>% filter_(~VersieLSVI == "Versie 3"))
Resultaat_kwal1 <- list(Resultaat[[1]] %>% filter_(~Kwaliteitsniveau == 1),
                          Resultaat[[2]] %>% filter_(~Kwaliteitsniveau == 1),
                          Resultaat[[3]] %>% filter_(~Kwaliteitsniveau == 1))


test_that("ConnectieLSVIhabitats is een open RODBC-connectie", {
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats = "geenConnectie", Versie = "alle",
                               Kwaliteitsniveau = "1",
                               Data_indicatoren,
                               Data_soorten),
               "ConnectieLSVIhabitats does not inherit from class RODBC")
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "1",
                                Data_indicatoren,
                                Data_soorten), 
               Resultaat_kwal1)
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("parameter versie heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",                     #eigenlijk moet kwaliteitsniveau alle nog getest worden, maar daarvoor functie eerst deftig uitwerken!
                           Kwaliteitsniveau = "1",
                           Data_indicatoren,
                           Data_soorten),
               Resultaat_kwal1)
  expect_equal(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "Versie 3",
                           Kwaliteitsniveau = "1",
                           Data_indicatoren,
                           Data_soorten),
               Resultaat_kwal1)
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = 2,
                           Kwaliteitsniveau = "alle",
                           Data_indicatoren,
                           Data_soorten),
               "Versie is not a string")
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("parameter kwaliteitsniveau heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = "1",
                           Data_indicatoren,
                           Data_soorten),
               Resultaat_kwal1)
  expect_equal(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = 1,
                           Data_indicatoren,
                           Data_soorten),
               Resultaat_kwal1)
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = "streefwaarde",
                           Data_indicatoren,
                           Data_soorten),
               "Kwaliteitsniveau moet een van de volgende waarden zijn")
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("dataframe Data_soorten heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                           Kwaliteitsniveau = "1",
                           Data_indicatoren,
                           Data_soorten),
               Resultaat_kwal1)
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_indicatoren,
                               Data_soorten %>%
                                 mutate_(
                                   Bedekking = ~ifelse(Bedekking == "5-10%", "foute invoer", Bedekking)
                                 )),
               "Niet alle waarden vermeld onder Data_soorten\\$Bedekking komen overeen met de bedekkingsschaal die gebruikt wordt voor deze monitoring.")
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                               Data_indicatoren,
                               Data_soorten %>%
                                 mutate_(
                                   Soort_Latijn = ~ifelse(Soort_Latijn == "Myrica gale",
                                                          "Myrca gale", Soort_Latijn)
                                 )),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_Latijn komen overeen met wetenschappelijke namen van soorten in de databank.")
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                               Data_indicatoren,
                               Data_soorten %>%
                                 mutate_(
                                   Habitatsubtype = ~"onbekend"
                                 )),
               "Niet alle waarden vermeld onder Data_soorten\\$Habitatsubtype zijn habitatsubtypes")
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("dataframe Data_indicatoren heeft correct formaat", {
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                                Data_indicatoren %>%
                                  mutate_(
                                    Waarde = ~ifelse(Waarde == "Rare", "zeven", Waarde)
                                  ),
                               Data_soorten),
               "Foute invoer in Data_voorwaarden\\$Waarde: niet alle categorische waarden komen overeen met het invoermasker uit de databank")
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                               Data_indicatoren %>%
                                 mutate_(
                                   Criterium = ~ifelse(Waarde == "Rare", "fout", Criterium)
                                 ),
                               Data_soorten),
               "Niet alle waarden vermeld onder Data_indicatoren\\$Criterium komen overeen met waarden vermeld in de databank")
  expect_error(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                               Kwaliteitsniveau = "alle",
                               Data_indicatoren %>%
                                 mutate_(
                                   Indicator = ~ifelse(Waarde == "Rare", "fout", Indicator)
                                 ),
                               Data_soorten),
               "Niet alle waarden vermeld onder Data_indicatoren\\$Indicator komen overeen met waarden vermeld in de databank")
  expect_warning(berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "alle",
                                Kwaliteitsniveau = "alle",
                               Data_indicatoren %>%
                                 mutate_(
                                   Waarde = ~ifelse(Waarde == "70-80%", NA, Waarde)
                                 ),
                               Data_soorten),
               "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

#werking childID nog testen!



