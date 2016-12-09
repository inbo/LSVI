#' @title Genereert LSVI-rapport op basis van de opgegeven parameters
#'
#' @description Deze functie genereert een rapport met habitatfiches die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de habitatsubtypes die voldoen aan de opgegeven parameters.  (Om een tabel te genereren met deze informatie om zelf een rapport te kunnen samenstellen, wordt verwezen naar de functie geefInfoHabitatfiche().)
#'
#' @template Zoekparameters
#'
#' @param Bestandsnaam Een naam voor het html-bestand dat gegenereerd wordt, bestaande uit een string die eindigt op '.html'
#' @inheritParams selecteerIndicatoren
#' @param verbose geeft de toestand van het systeem aan, om te zorgen dat boodschappen niet onnodig gegeven worden
#'
#' @return Deze functie genereert habitatfiches in de vorm van html-files die in de working directory opgeslagen worden.
#' 
#' @examples 
#' maakLSVIrapport(Bestandsnaam = "Habitatfiche_4030_versie3.html", 
#'                 Versie = "Versie 3", Habitatsubtype = "4010")
#'
#' @export
#'
#' @importFrom rmarkdown render
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom assertthat assert_that noNA is.flag
#' @importFrom cwhmisc cpos
#'
#'
maakLSVIrapport <- 
  function(Bestandsnaam = "LSVIrapport.html",
           Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Habitatgroep","Habitatgroepnaam"),  
           Habitattype = geefUniekeWaarden("Habitattype","Habitatcode"), 
           Habitatsubtype = geefUniekeWaarden("Habitatsubtype","Habitatcode_subtype"),
           verbose = TRUE){
    
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))
    assert_that(is.character(Bestandsnaam))
    assert_that(grepl(".html", Bestandsnaam))
    assert_that(cpos(Bestandsnaam, ".html")==nchar(Bestandsnaam)-4)
    
    render(system.file("LSVIrapport.Rmd", package = "LSVI"), 
           params = list(Versie = Versie, Habitatgroep = Habitatgroep,
                         Habitattype = Habitattype,
                         Habitatsubtype = Habitatsubtype),
           output_file = Bestandsnaam,
           output_dir = getwd())

    if(verbose){
      message(sprintf("Het rapport is opgeslagen in de working directory: %s", getwd()))
    }
    
    
  }

