#' @title Vertaalt Tansley-waarde naar bedekking in procent
#'
#' @description Deze functie vertaalt een ingevoerde vector van categorieen uit de Tansley-schaal naar bedekkingen in procent op basis van een tabel waarin de gemiddelde bedekkingen voor de schaalverdelingen gegeven zijn.
#' 
#' @param TansleyVector vector met percentages, dus getallen tussen 0 en 100, die omgezet moeten worden naar Tansley-waarden
#' 
#' @param Tansley tabel met kolommen 'Voluit' (= namen Tansley-schaal) en 'GemBedekking' (= klassemiddens van de bedekkingsschaal).  Default wordt de in het package meegeleverde tabel gebruikt (Tansley.csv in de map schaaltabellen).
#' 
#' @return Vector met een overeenkomstig bedekkingspercentage voor elke categorie vermeld in Tansleyvector
#' 
#' @examples 
#' vertaalTansleyPercentage(c("sporadisch", "frequent", "codominant"))
#' 
#' @export
#' 
#' @importFrom readr read_csv
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% left_join
#' 

vertaalTansleyPercentage <- 
  function(TansleyVector, 
           Tansley = read_csv(system.file("schaaltabellen/Tansley.csv", package = "LSVI"))){
    
    assert_that(inherits(Tansley, "data.frame"))
    assert_that(has_name(Tansley, "Voluit"))
    assert_that(has_name(Tansley, "GemBedekking"))
    assert_that(is.numeric(Tansley$GemBedekking))
    
    assert_that(is.vector(TansleyVector))
    assert_that(is.character(TansleyVector))
    TansleyVector <- tolower(TansleyVector)

    if(has_name(Tansley, "Afgekort")){
      if(max(TansleyVector %in% Tansley$Afgekort)){
        for(i in seq_len(length(TansleyVector))){
          TansleyVector[i] <- 
            ifelse(TansleyVector[i] %in% Tansley$Afgekort,
                   Tansley[!is.na(Tansley$Afgekort) & Tansley$Afgekort == TansleyVector[i],]$Voluit,
                   TansleyVector[i])
        }
      }
    }
    if(!all(TansleyVector %in% Tansley$Voluit)){
      stop(sprintf("Niet alle bedekkingen vermeld in de TansleyVector komen overeen met de Tansley-schaal (%s)",
                   paste(Tansley$Voluit, collapse = ", ")))
    }
    
    
    Resultaat <- data.frame(Tansley = TansleyVector, stringsAsFactors = FALSE) %>%
      left_join(Tansley, by = c("Tansley" = "Voluit"))
    
    return(Resultaat$GemBedekking)
    
  }
