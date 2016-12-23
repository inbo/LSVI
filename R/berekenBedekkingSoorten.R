#' @title Berekent de totale bedekking van de soorten van een bepaalde soortengroep die in een opname aanwezig is
#'
#' @description Deze hulpfunctie bepaalt de bedekking van de soorten van een bepaalde soortengroep dat aanwezig is in een bepaalde opname.  Omdat het een technische hulpfunctie is, wordt hier gebruik gemaakt van ID's uit de databank.  Beter is om gebruik te maken van de generieke functie berekenAnalyseVariabele waarbij het eenvoudiger is om de soortengroep te selecteren.
#'
#' 
#' @inheritParams selecteerIndicatoren
#' @param  Data_soorten Bedekkingen van de sleutelsoorten in de vorm van een data.frame met velden ID, Soort_NL of Soort_Latijn en Percentage (integer die de bedekking in procent aangeeft). (Eventueel zou hier ook de NBNTaxonVersionKey kunnen gebruikt worden.)
#' @inheritParams geefSoortenlijstInvoerniveau
#' 
#' 
#' @return Deze functie genereert de resultaten in de vorm van een tabel met voor elk ID en elke SoortenlijstID de totale bedekking van de soorten.
#' 
#' @examples 
#' library(readr)
#' Data_soorten <- 
#'     read_csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"))
#' Schaalomzetting <-
#'     read_csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"))
#' Data_soorten <- merge(Data_soorten, Schaalomzetting, 
#'                       by.x = "Bedekking", by.y = "Schaal_opname")
#' Soortengroeplijst <- "369,143"
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' berekenBedekkingSoorten(ConnectieLSVIhabitats, Data_soorten, Soortengroeplijst)
#' library(RODBC)
#' odbcClose(ConnectieLSVIhabitats)
#'
#' @export   
#'
#' @importFrom readr read_csv2
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% inner_join filter_ group_by_ ungroup summarise_ distinct_
#' @importFrom RODBC odbcClose
#'
#'
berekenBedekkingSoorten <- 
  function(ConnectieLSVIhabitats, Data_soorten, 
           Soortengroeplijst){
    
    assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))
    assert_that(inherits(Data_soorten, "data.frame"))
    assert_that(has_name(Data_soorten, "ID"))
    
    assert_that(has_name(Data_soorten, "Percentage"))
    assert_that(is.numeric(Data_soorten$Percentage))
    
    
    Bedekking_soorten <- 
      selecteerSoortenInOpname(ConnectieLSVIhabitats, Data_soorten, Soortengroeplijst) %>%
      group_by_(~ID, ~SoortengroepID) %>%
      summarise_(Waarde = ~ (1 - prod((100 - Percentage) /100, na.rm=TRUE)) * 100) %>%
      ungroup()
    
    
    return(as.data.frame(Bedekking_soorten))
    
  } 
