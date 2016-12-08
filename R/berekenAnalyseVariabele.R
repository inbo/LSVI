#' @title Berekent de opgegeven AnalyseVariabele van de opgegeven soortgroep voor de opname
#'
#' @description Deze functie bepaalt/berekent de opgegeven analysevariabele op basis van een opgegeven soortengroep voor de opgegeven opname.  Voorbeelden van analysevariabelen zijn 
#' - 'aantal_aanwezig' die het aantal soorten uit de opgegeven soortengroep telt die aanwezig zijn in de opname.
#' - 'aantal_frequent_aanwezig' die het aantal soorten uit de opgegeven soortengroep telt die minstens frequent aanwezig zijn in de opname.  (Analoge analysevariabelen bestaan voor alle categorieen uit de Tansley-schaal.)
#' - 'aantal_afwezig' die het aantal soorten uit de opgegeven soortengroep telt die niet aanwezig zijn in de opname, wat wil zeggen dat ze niet voorkomen in de opname of dat in de opname hun Tansley-bedekking als 'afwezig' gequoteerd is.
#' - 'bedekking_vegetatie' berekent voor de opname de totale bedekking van alle soorten uit de opgegeven soortengroep (in %).
#' - 'bedekking_vegetatie_Tansley' berekent voor de opname de totale bedekking van alle soorten uit de opgegeven soortengroep en zet deze om naar de Tansley-schaal
#' 
#' De berekening van andere analysevariabelen uit de databank ('grootte_grootste_vegetatievlek', 'aantal_aanwezig_kruidlaag' en 'bedekking_grondvlak') is sterk afhankelijk van de verzamelde gegevens en wordt daarom niet in een generieke functie gestoken.  Ze gebeurt voor elk van de verzamelde gegevens in de specifieke functie die op maat van de gegevens geschreven is.
#'
#' 
#' @param AnalyseVariabele Zie description voor voorbeelden.  
#' @param  Data_soorten Bedekkingen van de sleutelsoorten in de vorm van een data.frame met velden ID, Soort_NL of Soort_Latijn en Percentage (bedekking in procent) of Tansley (bedekking in Tansley-schaal), afhankelijk van het type AnalyseVariabele.  Voor een analysevariabele startend met 'aantal' moet Data_soorten het veld Tansley bevatten (en bij voorkeur ook Percentage als een soortenlijst gegeven wordt die voor de LSVI-berekening gegroepeerd moet worden tot een hoger niveau, bv. Genusniveau), voor een analysevariabele startend met 'bedekking' het veld Bedekking.
#' @inheritParams geefSoortenlijstSoortniveau
#' 
#' 
#' @return Deze functie genereert de resultaten in de vorm van een tabel met voor elk ID en elke SoortenlijstID het aantal soorten of de bedekking van de soorten.
#' 
#' @examples 
#' Data_soorten <- 
#'     read.csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"), 
#'               stringsAsFactors = FALSE)
#' Schaalomzetting <-
#'     read.csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"),
#'              stringsAsFactors = FALSE)
#' Data_soorten <- merge(Data_soorten, Schaalomzetting, 
#'                       by.x = "Bedekking", by.y = "Schaal_opname")
#'                       
#' berekenAnalyseVariabele("aantal_aanwezig", Data_soorten, "369,143")
#' berekenAnalyseVariabele("aantal_frequent_aanwezig", Data_soorten, "369,143")
#' berekenAnalyseVariabele("bedekking_vegetatie", Data_soorten, "369,143")
#' berekenAnalyseVariabele("bedekking_vegetatie_Tansley", Data_soorten, "369,143")
#'
#' @export   
#'
#' @importFrom utils read.csv2 read.csv
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter_
#'
#'
berekenAnalyseVariabele <- 
  function(AnalyseVariabele,
           Data_soorten, 
           Soortengroeplijst){
    assert_that(is.string(AnalyseVariabele))
    AnalyseVariabele <- tolower(AnalyseVariabele)
    Tansley <- read.csv(system.file("schaaltabellen/Tansley.csv", package = "LSVI"),
                        stringsAsFactors = FALSE) 
    Analysevariabelen <- sprintf("aantal_%s_aanwezig", 
                                 (tolower((Tansley %>% filter_(~ Voluit != "afwezig"))$Voluit)))
    Analysevariabelen <- 
      c(Analysevariabelen, "aantal_aanwezig", "aantal_afwezig", 
        "bedekking_vegetatie", "bedekking_vegetatie_tansley")
    if(!AnalyseVariabele %in% Analysevariabelen){
      stop(sprintf("De AnalyseVariabele is geen geldige waarde, geef een van volgende waarden op: %s",
                   paste(Analysevariabelen, collapse = ", ")))
    }
    
    if(grepl("aantal", AnalyseVariabele)){
      if(grepl("aantal_[[:alpha:]]*_aanwezig", AnalyseVariabele)){
        Minimumniveau <- gsub(
          pattern = "aantal_([[:alpha:]]*)_aanwezig",
          replacement = "\\1",
          x = AnalyseVariabele
        )
      } else {
        Minimumniveau <- gsub(
          pattern = "aantal_([[:alpha:]]*)",
          replacement = "\\1",
          x = AnalyseVariabele
        )
      }
      Resultaat <- berekenAantalSoorten(Data_soorten, Soortengroeplijst, Minimumniveau)
    } else {
      Resultaat <- berekenBedekkingSoorten(Data_soorten, Soortengroeplijst)
      if(grepl("tansley",AnalyseVariabele)){
        Resultaat$Tansley <- vertaalBedekkingTansley(Resultaat$Waarde)
        # Resultaat$Tansley <- NA
        # for(i in 1:nrow(Tansley)){
        #   Resultaat$Tansley <- 
        #     ifelse(Resultaat$Waarde >= Tansley[i,"Ondergrens"] & 
        #              Resultaat$Waarde < Tansley[i,"Bovengrens"],
        #            Tansley[i,"Voluit"],
        #            Resultaat$Tansley)
        # }
      }
    }
    
    return(as.data.frame(Resultaat))
  } 

