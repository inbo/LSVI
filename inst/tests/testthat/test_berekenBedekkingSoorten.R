context("test berekenBedekkingSoorten")

library(readr)
library(dplyr)

Data_soorten <- 
  read_csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"))
Schaalomzetting <- 
  read_csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"))
Data_soorten <- merge(Data_soorten, Schaalomzetting, 
                      by.x = "Bedekking", by.y = "Schaal_opname")
Soortengroeplijst <- "369,143"
berekenBedekkingSoorten(Data_soorten, Soortengroeplijst)
Resultaat <- data.frame(ID = c("Jo1380", "Jo1380", "WT0173", "WT0173", "WT0174", "WT0174"),
                                 SoortengroepID = c(143, 369, 143, 369, 143, 369),
                                 Waarde = c(60.51086,3,0.06,0,7.5,0),
                                 stringsAsFactors = FALSE)

test_that("Parameter Data_soorten heeft correct formaat", {
  expect_equal(berekenBedekkingSoorten(Data_soorten, Soortengroeplijst),
               Resultaat)
  expect_error(berekenBedekkingSoorten(Data_soorten %>% rename_(Veldnaam = ~ID), 
                                    Soortengroeplijst),
               'Error : Data_soorten does not have name ID\n')
  expect_error(berekenBedekkingSoorten(Data_soorten %>% select_(~ID, ~Percentage), 
                                    Soortengroeplijst),
               'Error : has_name\\(Data_soorten, "Soort_NL"\\) | has_name\\(Data_soorten, "Soort_Latijn"\\) is not TRUE\n')
  expect_error(berekenBedekkingSoorten(Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~ifelse(Soort_Latijn=="Myrica gale",
                                                               "Myrca gale",
                                                               Soort_Latijn)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_Latijn komen overeen met wetenschappelijke namen van soorten in de databank.")
  expect_error(berekenBedekkingSoorten(Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~NULL,
                                        Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                           "Eriophorum polystachion",
                                                           Soort_NL)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_NL komen overeen met Nederlandse namen van soorten in de databank.")
  expect_warning(berekenBedekkingSoorten(Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~ifelse(Soort_Latijn=="Drosera rotundifolia",
                                                                 "Quercus robur",
                                                                 Soort_Latijn)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenBedekkingSoorten(Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~NULL,
                                          Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                             "Zomereik",
                                                             Soort_NL)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_error(berekenBedekkingSoorten(Data_soorten %>% select_(~ID,~Soort_NL), 
                                    Soortengroeplijst),
               'Error : Data_soorten does not have name Percentage\n')
  expect_error(berekenBedekkingSoorten(Data_soorten %>% 
                                      mutate_(
                                        Percentage = ~ifelse(Percentage==3,
                                                          "3",
                                                          Percentage)
                                      ), 
                                    Soortengroeplijst),
               "Error : Data_soorten\\$Percentage is not a numeric or integer vector")
  
})


test_that("Parameter Soortengroeplijst heeft correct formaat", {
  expect_error(berekenBedekkingSoorten(Data_soorten, 
                                    Soortengroeplijst = 143),
               "Error : Soortengroeplijst\\$SoortengroepIDs is not a character vector\n")
  expect_equal(berekenBedekkingSoorten(Data_soorten, "143"),
               Resultaat %>% filter_(~SoortengroepID == 143))
})


Resultaat_370 <- data.frame(ID = c("Jo1380", "WT0173", "WT0174"),
                            SoortengroepID = c(370, 370, 370),
                            Waarde = c(61.695533,0.06,7.5),
                            stringsAsFactors = FALSE)


test_that("Gegevens in subniveau worden correct behandeld", {
  expect_equal(berekenBedekkingSoorten(Data_soorten, "370"),
               Resultaat_370)
  expect_equal(berekenBedekkingSoorten(Data_soorten %>% 
                                      mutate_(Soort_Latijn = ~NULL), 
                                    "370"),
               Resultaat_370)
  expect_warning(berekenBedekkingSoorten(Data_soorten %>%
                                        filter_(~grepl("Sphagnum", Soort_Latijn)),
                                      "370"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenBedekkingSoorten(Data_soorten %>%
                                        filter_(~grepl("Sphagnum", Soort_Latijn)) %>%
                                        mutate_(Soort_Latijn = ~NULL),
                                      "370"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenBedekkingSoorten(Data_soorten,"371"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(berekenBedekkingSoorten(Data_soorten %>%
                                        mutate_(Soort_Latijn = ~NULL),
                                      "371"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
})
