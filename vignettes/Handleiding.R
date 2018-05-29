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
geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "LSVIfiche")

## ------------------------------------------------------------------------
soortenlijst4010 <- geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "LSVIfiche")

## ----eval=FALSE----------------------------------------------------------
#  library(readr)
#  write_delim(soortenlijst4010, "C:/R/soortenlijst4010.csv", delim = ";")

## ----eval=FALSE----------------------------------------------------------
#  library(readr)
#  Dataset <- read_delim("Gegevens.csv", delim = ";")

