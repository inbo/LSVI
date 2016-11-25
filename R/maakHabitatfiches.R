#' @title Genereert habitatfiche(s) van LSVI op basis van de opgegeven parameters
#'
#' @description Deze functie genereert habitatfiches die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de habitatsubtypes die voldoen aan de opgegeven parameters.  
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Default is telkens "alle", waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#'De gegenereerde habitatfiches worden opgeslagen in de folder die als working directory gespecifieerd is.
#'
#' @inheritParams selecteerIndicatoren
#' @param verbose geeft de toestand van het systeem aan, om te zorgen dat boodschappen niet onnodig gegeven worden
#'
#' @return Deze functie genereert habitatfiches in de vorm van html-files die in de workspace opgeslagen worden.
#' 
#' @examples 
#' maakHabitatfiches(Versie = "Versie 3", Habitatsubtype = "4010")
#'
#' @export
#'
#' @importFrom rmarkdown render
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom assertthat assert_that noNA is.flag
#'
#'
maakHabitatfiches <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Habitatgroep","Habitatgroepnaam"),  
           Habitattype = geefUniekeWaarden("Habitattype","Habitatcode"), 
           Habitatsubtype = geefUniekeWaarden("Habitatsubtype","Habitatcode_subtype"),
           verbose = TRUE){
    
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))
    
    Indicatoren <- selecteerIndicatoren(Versie, Habitatgroep, Habitattype, Habitatsubtype)
    
    for(versie in unique(Indicatoren$Versie)){
      for(habitatsubtype in unique(as.character(Indicatoren$Habitatsubtype))){
        Bestandnaam <- sprintf("Habitatfiche_%s_%s.html",
                               habitatsubtype,
                               sub(versie, 
                                   pattern = " ", 
                                   replacement = ""))
        render(system.file("Habitatfiche.Rmd", package = "LSVI"), 
               params = list(Versie = versie, Habitatsubtype = habitatsubtype),
               output_file = Bestandnaam,
               output_dir = getwd())
      }
    }
    if(verbose){
      message(sprintf("De fiche(s) is/zijn opgeslagen in de working directory: %s", getwd()))
    }
    
    
  }

