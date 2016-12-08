#' @title Vertaalt bedekking in procent naar Tansley-waarde
#'
#' @description Deze functie vertaalt een ingevoerde vector van getallen (bedekkingen in procent) naar de categorieen uit de Tansley-schaal op basis van een tabel waarin onder- en bovengrenzen voor de schaalverdeling gegeven zijn.
#' 
#' @param Percentages vector met percentages, dus getallen tussen 0 en 100, die omgezet moeten worden naar Tansley-waarden
#' 
#' @param Tansley tabel met kolommen 'Voluit' (= namen Tansley-schaal), 'Ondergrens' en 'Bovengrens'.  Default wordt de in het package meegeleverde tabel gebruikt (Tansley.csv in de map schaaltabellen).
#' 
#' @return Vector met de Tansley-waarden van de ingevoerde percentages
#' 
#' @examples 
#' vertaalBedekkingTansley(c(2.5, 10, 30))
#' 
#' @export
#' 
#' @importFrom readr read_csv
#' @importFrom assertthat assert_that has_name
#' 

vertaalBedekkingTansley <- 
  function(Percentages, 
           Tansley = read_csv(system.file("schaaltabellen/Tansley.csv", package = "LSVI"))){
    assert_that(is.vector(Percentages))
    assert_that(is.numeric(Percentages))
    if(!all(Percentages >=0 & Percentages <= 100)){
      stop("Niet alle ingegeven percentages liggen tussen 0 en 100 %")
    }
    
    assert_that(inherits(Tansley, "data.frame"))
    assert_that(has_name(Tansley, "Voluit"))
    assert_that(has_name(Tansley, "Ondergrens"))
    assert_that(is.numeric(Tansley$Ondergrens))
    assert_that(has_name(Tansley, "Bovengrens"))
    assert_that(is.numeric(Tansley$Bovengrens))
    
    Resultaat <- rep(NA, length(seq_len(length(Percentages))))
    for(teller in seq_len(length(Percentages))){
      for(i in seq_len(nrow(Tansley))){
        Resultaat[teller] <- 
          unlist(ifelse(Percentages[teller] >= Tansley[i,"Ondergrens"] & 
                 Percentages[teller] < Tansley[i,"Bovengrens"],
                 Tansley[i,"Voluit"],
                 Resultaat[teller]))
      }
    }
    
    return(Resultaat)

  }