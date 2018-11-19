## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
packageDescription(pkg = "LSVI")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("inbo/LSVI@develop", build_vignettes = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  ?maakLSVIrapport

## ------------------------------------------------------------------------
library(LSVI)
maakConnectiePool()
geefSoortenlijst(Habitattype = "4030", Taxonlijsttype = "LSVIfiche")

## ------------------------------------------------------------------------
soortenlijst4030 <- geefSoortenlijst(Habitattype = "4030", Taxonlijsttype = "LSVIfiche")

## ----eval=FALSE----------------------------------------------------------
#  library(readr)
#  write_delim(soortenlijst4030, "C:/R/soortenlijst4030.csv", delim = ";")

## ----eval=FALSE----------------------------------------------------------
#  library(readr)
#  Dataset <- read_delim("Gegevens.csv", delim = ";")

## ------------------------------------------------------------------------
#Eerst worden wat voorbeeldgegevens opgehaald uit het package:
library(readr)
Data_habitat <-
  read_delim(
    system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
    delim = ";",
    col_types = list(col_character(), col_character(), col_character())
  )
Data_voorwaarden <-
  read_delim(
    system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"),
    delim = ";"
  )
Data_soortenKenmerken <-
  read_delim(
    system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI"),
    delim = ";"
  )

#En hier gebeurt de berekening zelf:
berekenLSVIbasis(
  Versie = "Versie 2.0",
  Kwaliteitsniveau = "1", Data_habitat,
  Data_voorwaarden, Data_soortenKenmerken
)

## ------------------------------------------------------------------------
resultaat <-
  berekenLSVIbasis(
    Versie = "Versie 2.0",
    Kwaliteitsniveau = "1", Data_habitat,
    Data_voorwaarden, Data_soortenKenmerken
  )

## ------------------------------------------------------------------------
resultaat$Resultaat_globaal  #ofwel: resultaat[["Resultaat_globaal"]]
resultaat$Resultaat_criterium
resultaat$Resultaat_indicator
resultaat$Resultaat_detail

## ----eval=FALSE----------------------------------------------------------
#  library(LSVI)
#  maakConnectiePool()
#  #Nu kan je alle functies gebruiken zonder expliciet het argument ConnectieLSVIhabitats op te geven, bv.
#  geefVersieInfo()
#  geefSoortenlijst(Habitattype = "4030")
#  
#  #En als je geen functies meer nodig hebt uit het LSVI-package, kan je de ConnectiePool afsluiten en verwijderen:
#  library(pool)
#  poolClose(ConnectiePool)
#  rm(ConnectiePool)

## ----eval=FALSE----------------------------------------------------------
#  library(LSVI)
#  Connectie <- connecteerMetLSVIdb()
#  #Nu moet je bij elke functie opnieuw deze connectie meegeven
#  geefVersieInfo(ConnectieLSVIhabitats = Connectie)
#  geefSoortenlijst(Habitattype = "4030", ConnectieLSVIhabitats = Connectie)
#  
#  #De connectie sluiten kan met dbDisconnect uit het DBI-package:
#  library(DBI)
#  dbDisconnect(Connectie)

