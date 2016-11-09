context("test berekenAantalSoorten")

library(dplyr)

Data_soorten <- 
  read.csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"), 
            stringsAsFactors = FALSE)
Schaalomzetting <- 
  read.csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"),
            stringsAsFactors = FALSE)
Data_soorten <- merge(Data_soorten, Schaalomzetting, 
                      by.x = "Bedekking", by.y = "Schaal_opname")
Soortengroeplijst <- data.frame(Niveau = 1, SoortengroepIDs = "369,143",
                                stringsAsFactors = FALSE)
berekenAantalSoorten(Data_soorten, Soortengroeplijst)
resultaat_aanwezig <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0174"),
                                 SoortengroepID = c(143, 369, 143, 143),
                                 Aantal = c(4,1,1,1),
                                 stringsAsFactors = FALSE)
berekenAantalSoorten(Data_soorten, Soortengroeplijst, Minimumniveau = "Frequent")
resultaat_frequent <- data.frame(ID = c("Jo1380", "Jo1380", "WT0174"),
                                 SoortengroepID = c(143, 369, 143),
                                 Aantal = c(3,1,1),
                                 stringsAsFactors = FALSE)
resultaat_afwezig <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
                                 SoortengroepID = c(143, 369, 143, 369, 143, 369),
                                 Aantal = c(8,4,11,5,11,5),
                                 stringsAsFactors = FALSE)


test_that("Parameter Data_soorten heeft correct formaat", {
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst),
               resultaat_aanwezig)
  expect_error(berekenAantalSoorten(Data_soorten %>% rename_(Veldnaam = ~ID), 
                                    Soortengroeplijst),
               'Error : has_name\\(x = Data_soorten, name = "ID"\\) is not TRUE\n')
  expect_error(berekenAantalSoorten(Data_soorten %>% select_(~ID, ~Tansley), 
                                    Soortengroeplijst),
               'Error : has_name\\(Data_soorten, "Soort_NL"\\) | has_name\\(Data_soorten, "Soort_Latijn"\\) is not TRUE\n')
  expect_error(berekenAantalSoorten(Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~ifelse(Soort_Latijn=="Myrica gale",
                                                               "Myrca gale",
                                                               Soort_Latijn)
                                        ), 
                                    Soortengroeplijst),
               "* Niet alle waarden vermeld onder Data_soorten\\$Soort_Latijn komen overeen met wetenschappelijke namen van soorten in de databank.")
  expect_error(berekenAantalSoorten(Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~NULL,
                                        Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                               "Eriophorum polystachion",
                                                               Soort_NL)
                                      ), 
                                    Soortengroeplijst),
               "* Niet alle waarden vermeld onder Data_soorten\\$Soort_NL komen overeen met Nederlandse namen van soorten in de databank.")
  expect_warning(berekenAantalSoorten(Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~ifelse(Soort_Latijn=="Drosera rotundifolia",
                                                                 "Quercus robur",
                                                                 Soort_Latijn)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenAantalSoorten(Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~NULL,
                                          Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                                 "Zomereik",
                                                                 Soort_NL)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_error(berekenAantalSoorten(Data_soorten %>% select_(~ID,~Soort_NL), 
                                    Soortengroeplijst),
               'Error : has_name\\(x = Data_soorten, name = "Tansley"\\) is not TRUE\n')
  expect_equal(berekenAantalSoorten(Data_soorten %>% 
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "Abundant",
                                                          Tansley)
                                      ), 
                                    Soortengroeplijst),
               resultaat_aanwezig)
  expect_equal(berekenAantalSoorten(Data_soorten %>%
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "A",
                                                          Tansley)
                                      ),
                                    Soortengroeplijst),
               resultaat_aanwezig)
  expect_equal(berekenAantalSoorten(Data_soorten %>%
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "a",
                                                          Tansley)
                                      ),
                                    Soortengroeplijst),
               resultaat_aanwezig)
  expect_error(berekenAantalSoorten(Data_soorten %>% 
                                      mutate_(
                                        Tansley = ~ifelse(Tansley=="abundant",
                                                          "foute invoer",
                                                          Tansley)
                                      ), 
                                    Soortengroeplijst),
               "* Niet alle bedekkingen vermeld onder Data_soorten\\$Tansley komen overeen met de Tansley-schaal *")
  
})


test_that("Parameter Soortengroeplijst heeft correct formaat", {
  expect_error(berekenAantalSoorten(Data_soorten, 
                                    Soortengroeplijst = 
                                      data.frame(Niveau = 1, 
                                                 SoortengroepIDs = 143)),
               "Error : Soortengroeplijst\\$SoortengroepIDs is not a character vector\n")
  expect_equal(berekenAantalSoorten(Data_soorten, 
                                    data.frame(Niveau = 1, 
                                               SoortengroepIDs = "143",
                                               stringsAsFactors = FALSE)),
               resultaat_aanwezig %>% filter_(~SoortengroepID == 143))
  expect_error(berekenAantalSoorten(Data_soorten, 
                                    data.frame(Niveau = "1", 
                                               SoortengroepIDs = "143",
                                               stringsAsFactors = FALSE)),
               "Error : Soortengroeplijst\\$Niveau\\[i\\] is not a count \\(a single positive integer\\)\n")
  expect_error(berekenAantalSoorten(Data_soorten, 
                                    c(1,"369,143")),
               "Error : Soortengroeplijst does not inherit from class data.frame\n")
})


test_that("Parameter Minimumniveau heeft correct formaat", {
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "Aanwezig"),
               resultaat_aanwezig)
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "frequent"),
               resultaat_frequent)
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "FREQUENT"),
               resultaat_frequent)
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "f"),
               resultaat_frequent)
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "F"),
               resultaat_frequent)
  expect_equal(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "afwezig"),
               resultaat_afwezig)
  expect_error(berekenAantalSoorten(Data_soorten, Soortengroeplijst, "blabla"),
               "* Foute invoer voor Minimumniveau. *")
})

