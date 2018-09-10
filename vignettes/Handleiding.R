## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
packageDescription(pkg = "LSVI")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("inbo/LSVI", build_vignettes = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  ?maakLSVIrapport

## ------------------------------------------------------------------------
library(LSVI)
geefSoortenlijst(Habitattype = "4010", Taxonlijsttype = "LSVIfiche")

## ------------------------------------------------------------------------
soortenlijst4010 <- geefSoortenlijst(Habitattype = "4010", Taxonlijsttype = "LSVIfiche")

## ----eval=FALSE----------------------------------------------------------
#  library(readr)
#  write_delim(soortenlijst4010, "C:/R/soortenlijst4010.csv", delim = ";")

## ----eval=FALSE----------------------------------------------------------
#  library(readr)
#  Dataset <- read_delim("Gegevens.csv", delim = ";")

## ------------------------------------------------------------------------
#Eerst worden wat voorbeeldgegevens opgehaald uit het package:
library(readr)
Data_habitat <-
  read_delim(
    system.file("vbdata/opname4030habitat.csv", package = "LSVI"),
    delim = ";",
    col_types = list(col_character(), col_character(), col_character())
  )
Data_voorwaarden <-
  read_delim(
    system.file("vbdata/opname4030voorwaarden.csv", package = "LSVI"),
    delim = ";"
  )
Data_soortenKenmerken <-
  read_delim(
    system.file("vbdata/opname4030soortenKenmerken.csv", package = "LSVI"),
    delim = ";"
  )

#En hier gebeurt de berekening zelf:
berekenLSVIbasis(
  Versie = "Versie 3",
  Kwaliteitsniveau = "1", Data_habitat,
  Data_voorwaarden, Data_soortenKenmerken
)

## ------------------------------------------------------------------------
resultaat <-
  berekenLSVIbasis(
    Versie = "Versie 3",
    Kwaliteitsniveau = "1", Data_habitat,
    Data_voorwaarden, Data_soortenKenmerken
  )

## ------------------------------------------------------------------------
resultaat$Resultaat_globaal  #ofwel: resultaat[["Resultaat_globaal"]]
resultaat$Resultaat_criterium
resultaat$Resultaat_indicator
resultaat$Resultaat_detail

