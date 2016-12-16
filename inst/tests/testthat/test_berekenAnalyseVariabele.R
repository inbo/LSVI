context("test berekenAnalyseVariabele")

library(dplyr)
library(readr)

ConnectieLSVIhabitats <- connecteerMetLSVIdb()
Data_soorten <- 
  read_csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"))
Schaalomzetting <- 
  read_csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"))
Data_soorten <- merge(Data_soorten, Schaalomzetting, 
                      by.x = "Bedekking", by.y = "Schaal_opname")
Soortengroeplijst <- "369,143"

resultaat_aanwezig <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
                                 SoortengroepID = c(143, 369, 143, 369, 143, 369),
                                 Waarde = c(4,1,1,0,1,0),
                                 stringsAsFactors = FALSE)
resultaat_frequent <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
                                 SoortengroepID = c(143, 369, 143, 369, 143, 369),
                                 Waarde = c(3,1,0,0,1,0),
                                 stringsAsFactors = FALSE)
resultaat_afwezig <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
                                SoortengroepID = c(143, 369, 143, 369, 143, 369),
                                Waarde = c(8,4,11,5,11,5),
                                stringsAsFactors = FALSE)
resultaat_bedekking <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
                                  SoortengroepID = c(143, 369, 143, 369, 143, 369),
                                  Waarde = c(60.51086,3,0.06,0,7.5,0),
                                  stringsAsFactors = FALSE)
resultaat_bedekking_tansley <-
  data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
             SoortengroepID = c(143, 369, 143, 369, 143, 369),
             Waarde = c(60.51086,3,0.06,0,7.5,0),
             Tansley = c("dominant", "frequent", "sporadisch", "afwezig", "abundant", "afwezig"),
             stringsAsFactors = FALSE)

test_that("ConnectieLSVIhabitats is een open RODBC-connectie", {
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats = "geenConnectie", 
                                    "aantal_aanwezig",
                                    Data_soorten, Soortengroeplijst),
               "ConnectieLSVIhabitats does not inherit from class RODBC")
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats,"aantal_aanwezig",Data_soorten, 
                                       Soortengroeplijst),
               resultaat_aanwezig)
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

test_that("Parameter Data_soorten heeft correct formaat", {
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats,"aantal_aanwezig",Data_soorten, 
                                       Soortengroeplijst),
               resultaat_aanwezig)
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% rename_(Veldnaam = ~ID), 
                                    Soortengroeplijst),
               'Data_soorten does not have name ID')
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% select_(~ID, ~Tansley), 
                                    Soortengroeplijst),
               'has_name\\(Data_soorten, "Soort_NL"\\) | has_name\\(Data_soorten, "Soort_Latijn"\\) is not TRUE')
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~ifelse(Soort_Latijn=="Myrica gale",
                                                               "Myrca gale",
                                                               Soort_Latijn)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_Latijn komen overeen met wetenschappelijke namen van soorten in de databank.")
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~NULL,
                                        Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                           "Eriophorum polystachion",
                                                           Soort_NL)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_NL komen overeen met Nederlandse namen van soorten in de databank.")
  expect_warning(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                         Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~ifelse(Soort_Latijn=="Drosera rotundifolia",
                                                                 "Quercus robur",
                                                                 Soort_Latijn)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                         Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~NULL,
                                          Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                             "Zomereik",
                                                             Soort_NL)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% select_(~ID,~Soort_NL), 
                                    Soortengroeplijst),
               'Data_soorten does not have name Tansley')
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% 
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "Abundant",
                                                          Tansley)
                                      ), 
                                    Soortengroeplijst),
               resultaat_aanwezig)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>%
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "A",
                                                          Tansley)
                                      ),
                                    Soortengroeplijst),
               resultaat_aanwezig)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>%
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "a",
                                                          Tansley)
                                      ),
                                    Soortengroeplijst),
               resultaat_aanwezig)
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten %>% 
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "foute invoer",
                                                          Tansley)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle bedekkingen vermeld onder Data_soorten\\$Tansley komen overeen met de Tansley-schaal")
  
})


test_that("Parameter Soortengroeplijst heeft correct formaat", {
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",Data_soorten, 
                                    Soortengroeplijst = 143),
               "Soortengroeplijst\\$SoortengroepIDs is not a character vector")
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                       Data_soorten, "143"),
               resultaat_aanwezig %>% filter_(~SoortengroepID == 143))
})


test_that("Parameter AnalyseVariabele heeft correct formaat", {
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats,"aantal_aanwezig",Data_soorten, 
                                       Soortengroeplijst),
               resultaat_aanwezig)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_frequent_aanwezig",
                                       Data_soorten, Soortengroeplijst),
               resultaat_frequent)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_FREQUENT_aanwezig",
                                       Data_soorten, Soortengroeplijst),
               resultaat_frequent)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_afwezig",Data_soorten, 
                                       Soortengroeplijst),
               resultaat_afwezig)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "bedekking_vegetatie",
                                       Data_soorten, Soortengroeplijst),
               resultaat_bedekking)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "bedekking_vegetatie_Tansley",
                                       Data_soorten, Soortengroeplijst),
               resultaat_bedekking_tansley)
  expect_error(berekenAnalyseVariabele(ConnectieLSVIhabitats, "Foute invoer", Data_soorten, 
                                       Soortengroeplijst),
               "De AnalyseVariabele is geen geldige waarde, geef een van volgende waarden op:")
})


Resultaat_370 <- data.frame(ID = c("Jo1380", "WT0173", "WT0174"),
                            SoortengroepID = c(370, 370, 370),
                            Waarde = c(2,1,1),
                            stringsAsFactors = FALSE)

Resultaat_370_frequent <- data.frame(ID = c("Jo1380", "WT0173", "WT0174"),
                                     SoortengroepID = c(370, 370, 370),
                                     Waarde = c(2,0,1),
                                     stringsAsFactors = FALSE)


test_that("Gegevens in subniveau worden correct behandeld", {
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig", 
                                       Data_soorten, "370"),
               Resultaat_370)
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig", 
                                       Data_soorten %>% 
                                      mutate_(Soort_Latijn = ~NULL), 
                                    "370"),
               Resultaat_370)
  expect_warning(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                         Data_soorten %>%
                                        filter_(~grepl("Sphagnum", Soort_Latijn)),
                                      "370"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                         Data_soorten %>%
                                        filter_(~grepl("Sphagnum", Soort_Latijn)) %>%
                                        mutate_(Soort_Latijn = ~NULL),
                                      "370"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                         Data_soorten,"371"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_aanwezig",
                                         Data_soorten %>%
                                        mutate_(Soort_Latijn = ~NULL),
                                      "371"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_equal(berekenAnalyseVariabele(ConnectieLSVIhabitats, "aantal_frequent_aanwezig",
                                       Data_soorten, "370"),
               Resultaat_370_frequent)
})

library(RODBC)
odbcClose(ConnectieLSVIhabitats)
