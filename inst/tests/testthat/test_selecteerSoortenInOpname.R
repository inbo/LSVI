context("test selecteerSoortenInOpname")

library(readr)
library(dplyr)

ConnectieLSVIhabitats <- connecteerMetLSVIdb()
Data_soorten <- 
  read_csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"))
Soortengroeplijst <- "369,143"

Soortengroep <- 
  geefSoortenlijstInvoerniveau(ConnectieLSVIhabitats, 
                               data.frame(Niveau = 1,
                                          SoortengroepIDs = Soortengroeplijst,
                                          stringsAsFactors = FALSE)) %>%
  select_(
    ~ SoortengroepID,
    ~ SoortensubgroepID,
    ~ WetNaamKort,
    ~ NedNaam
  )

Resultaat <- Data_soorten %>%
  mutate_(
    WetNaamKort = ~ gsub(
      pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
      replacement = "\\1 \\2",
      x = Soort_Latijn
    )
  ) %>%
  inner_join(Soortengroep, by = c("WetNaamKort" = "WetNaamKort"))

library(RODBC)
odbcClose(ConnectieLSVIhabitats)

test_that("ConnectieLSVIhabitats is een open RODBC-connectie", {
  expect_error(selecteerSoortenInOpname(ConnectieLSVIhabitats = "geenConnectie", 
                                    Data_soorten, Soortengroeplijst),
               "ConnectieLSVIhabitats does not inherit from class RODBC")
  ConnectieLSVIhabitats <- connecteerMetLSVIdb()
  expect_equal(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten, Soortengroeplijst),
               Resultaat)
  library(RODBC)
  odbcClose(ConnectieLSVIhabitats)
})

ConnectieLSVIhabitats <- connecteerMetLSVIdb()

test_that("Parameter Data_soorten heeft correct formaat", {
  expect_equal(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten, Soortengroeplijst),
               Resultaat)
  expect_error(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                        Data_soorten %>% rename_(Veldnaam = ~ID), 
                                    Soortengroeplijst),
               'Data_soorten does not have name ID')
  expect_error(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                        Data_soorten %>% select_(~ID, ~Bedekking), 
                                    Soortengroeplijst),
               'has_name\\(Data_soorten, "Soort_NL"\\) | has_name\\(Data_soorten, "Soort_Latijn"\\) is not TRUE')
  expect_error(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                        Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~ifelse(Soort_Latijn=="Myrica gale",
                                                               "Myrca gale",
                                                               Soort_Latijn)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_Latijn komen overeen met wetenschappelijke namen van soorten in de databank")
  expect_error(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                        Data_soorten %>% 
                                      mutate_(
                                        Soort_Latijn = ~NULL,
                                        Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                           "Eriophorum polystachion",
                                                           Soort_NL)
                                      ), 
                                    Soortengroeplijst),
               "Niet alle waarden vermeld onder Data_soorten\\$Soort_NL komen overeen met Nederlandse namen van soorten in de databank")
  expect_warning(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                          Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~ifelse(Soort_Latijn=="Drosera rotundifolia",
                                                                 "Quercus robur",
                                                                 Soort_Latijn)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                          Data_soorten %>%
                                        mutate_(
                                          Soort_Latijn = ~NULL,
                                          Soort_NL = ~ifelse(Soort_NL=="Veenpluis",
                                                             "Zomereik",
                                                             Soort_NL)
                                        ),
                                      Soortengroeplijst),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
 
})


test_that("Parameter Soortengroeplijst heeft correct formaat", {
  expect_error(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten, 
                                    Soortengroeplijst = 143),
               "Soortengroeplijst\\$SoortengroepIDs is not a character vector")
  expect_equal(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten, "143"),
               Resultaat %>% filter_(~SoortengroepID == 143))

})


test_that("Soorten in subniveau worden correct opgehaald", {
  expect_equal(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten, "370"),
               Resultaat %>%
                 mutate_(
                   SoortensubgroepID = ~ SoortengroepID,
                   SoortengroepID = as.integer(370)
                 ))
  expect_equal(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten %>% 
                                          mutate_(Soort_Latijn = ~NULL), 
                                        "370"),
               Resultaat %>%
                 mutate_(
                   SoortensubgroepID = ~ SoortengroepID,
                   SoortengroepID = as.integer(370),
                   Soort_Latijn = ~ NULL,
                   NedNaam = ~NULL
                 ) %>%
                 select_(
                   ~ID, ~Habitatsubtype, ~Soort_NL, ~Bedekking,
                   ~SoortengroepID, ~SoortensubgroepID, ~WetNaamKort
                 ))
  expect_warning(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten[1:36,],"370"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                          Data_soorten[1:36,] %>%
                                            mutate_(Soort_Latijn = ~NULL),
                                          "370"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten,"371"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
  expect_warning(selecteerSoortenInOpname(ConnectieLSVIhabitats, 
                                          Data_soorten %>%
                                            mutate_(Soort_Latijn = ~NULL),
                                          "371"),
                 "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
})

library(RODBC)
odbcClose(ConnectieLSVIhabitats)
