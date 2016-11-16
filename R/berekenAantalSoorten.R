#' @title Berekent het aantal soorten van een bepaalde soortengroep die in een opname aanwezig is
#'
#' @description Deze hulpfunctie bepaalt het aantal soorten van een bepaalde soortengroep dat aanwezig is in een bepaalde opname.  Alternatief kunnen ook het aantal soorten bepaald worden dat minstens frequent of abundant aanwezig is, of elke andere mogelijke categorie uit de Tansley-schaal (aan te geven in parameter Minimumniveau).  Omdat het een technische hulpfunctie is, wordt hier gebruik gemaakt van ID's uit de databank.  Beter is om gebruik te maken van de generieke functie berekenAnalyseVariabele die meer mogelijkheden heeft  voor het selecteren van de soortengroep.
#'
#' 
#' @param  Data_soorten Bedekkingen van de sleutelsoorten in de vorm van een data.frame met velden ID, Soort_NL of Soort_Latijn en Tansley (bedekking in Tansley-schaal). (Eventueel zou hier ook de NBNTaxonVersionKey kunnen gebruikt worden.)
#' @inheritParams geefSoortenlijstInvoerniveau
#' @param Minimumniveau Minimum bedekking voor een soort om meegeteld te worden.  Standaard worden alle aanwezige soorten meegeteld, maar er kan bv. ook gekozen worden om enkel soorten te tellen die ze minstens frequent of abundant aanwezig zijn.  De waarde 'afwezig' telt enkel de afwezige soorten.
#' 
#' 
#' @return Deze functie genereert de resultaten in de vorm van een tabel met voor elk ID en elke SoortenlijstID het aantal soorten.
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
#' Soortengroeplijst <- "369,143"
#' berekenAantalSoorten(Data_soorten, Soortengroeplijst)
#' berekenAantalSoorten(Data_soorten, Soortengroeplijst, Minimumniveau = "Frequent")
#'
#' @export   
#'
#' @importFrom utils read.csv2 read.csv
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% inner_join filter_ group_by_ ungroup summarise_ distinct_
#'
#'
berekenAantalSoorten <- 
  function(Data_soorten, 
           Soortengroeplijst, 
           Minimumniveau = "aanwezig"){
    assert_that(inherits(Data_soorten, "data.frame"))
    assert_that(has_name(Data_soorten, "ID"))
    
    assert_that(has_name(Data_soorten, "Tansley"))
    Tansley <- read.csv(system.file("schaaltabellen/Tansley.csv", package = "LSVI"),
                                       stringsAsFactors = FALSE) %>%
      select_(~Voluit, ~Afgekort)
    Data_soorten$Tansley <- tolower(Data_soorten$Tansley)
    if(!all(Data_soorten$Tansley %in% c(Tansley$Voluit, Tansley$Afgekort))){
      stop(sprintf("Niet alle bedekkingen vermeld onder Data_soorten$Tansley komen overeen met de Tansley-schaal (%s)",
           paste(Tansley$Voluit, collapse = ", ")))
    }
    if(!all(Data_soorten$Tansley %in% Tansley$Voluit)){
      Data_soorten <- Data_soorten %>%
        left_join(Tansley, by = c("Tansley" = "Afgekort")) %>%
        mutate_(
          Tansley = ~ifelse(Tansley %in% Tansley$Voluit,
                            Tansley,
                            Voluit)
        )
    }
    Data_soorten$TansleyF <- factor(Data_soorten$Tansley, levels = Tansley$Voluit)
    
    Minimumniveau <- tolower(Minimumniveau)
    if(!Minimumniveau %in% c("aanwezig", Tansley$Voluit, Tansley$Afgekort)){
      stop(sprintf("Foute invoer voor Minimumniveau.  Kies uit %s",
                   paste(c("aanwezig", Tansley$Voluit), collapse = ", ")))
    }
    Minimumniveau <- 
      ifelse(Minimumniveau == "aanwezig", "sporadisch",
             ifelse(Minimumniveau %in% Tansley$Voluit,
                     Minimumniveau,
                     Tansley[!is.na(Tansley$Afgekort) & (Tansley$Afgekort == Minimumniveau),]$Voluit)) 
    MinimumniveauF <- factor(Minimumniveau, levels = Tansley$Voluit)      
    
    
    Data_soorten <- selecteerSoortenInOpname(Data_soorten, Soortengroeplijst) %>%
      mutate_(
        SoortensubgroepID = ~ ifelse(is.na(SoortensubgroepID),
                                     WetNaamKort,
                                     SoortensubgroepID)
      ) %>%
      group_by_(~ID, ~SoortengroepID, ~SoortensubgroepID) %>%
      summarise_(TansleyF = ~max(as.numeric(TansleyF))) %>%
      ungroup()

    
    if(MinimumniveauF == "afwezig"){
      Aantal_soorten <- Data_soorten %>%
        filter_(~as.numeric(TansleyF) == as.numeric(MinimumniveauF)) %>%
        group_by_(~ID, ~SoortengroepID) %>%
        summarise_(Aantal = ~n()) %>%
        ungroup()
    } else {
      Aantal_soorten <- Data_soorten %>%
        filter_(~as.numeric(TansleyF) >= as.numeric(MinimumniveauF)) %>%
        group_by_(~ID, ~SoortengroepID) %>%
        summarise_(Aantal = ~n()) %>%
        ungroup()
    }
    
    Alle_waarnemingen <- Data_soorten %>%
      select_(~ID, ~SoortengroepID) %>%
      distinct_() %>%
      mutate_(Aantal = 0) %>%
      bind_rows(Aantal_soorten) %>%
      group_by_(~ID, ~SoortengroepID) %>%
      summarise_(
        Aantal = ~sum(Aantal)
      ) %>%
      ungroup()
    
    return(as.data.frame(Alle_waarnemingen))

  } 
