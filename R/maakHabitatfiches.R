#'
#'Genereert habitatfiche(s) van LSVI op basis van de opgegeven parameters
#'
#'Deze functie genereert habitatfiches die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de habitatsubtypes die voldoen aan de opgegeven parameters.  
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Voor eenzelfde parameter twee of meer waarden opgeven kan door de waarden te scheiden door 'or' en het geheel tussen haakjes te zetten.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#'De gegenereerde habitatfiches worden opgeslagen in de folder die als working directory gespecifieerd is.
#'
#' @inheritParams geefSoortenlijst
#'
#'@return Habitatfiches in de vorm van html-files die in de workspace opgeslagen worden.
#'
#'@export
#'
#'@importFrom rmarkdown render
#'@importFrom RODBC sqlQuery odbcClose
#'
#'
maakHabitatfiches <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Habitatgroep","Habitatgroepnaam"),  
           Habitattype = geefUniekeWaarden("Habitattype","Habitatcode"), 
           Habitatsubtype = geefUniekeWaarden("Habitatsubtype","Habitatcode_subtype")){
    match.arg(Versie)
    match.arg(Habitatgroep)
    match.arg(Habitattype)
    match.arg(Habitatsubtype)
    
    #eerst de selectiegegevens ophalen en de nodige gegevens uit tabel Indicator_habitat
    Parametervoorwaarde <- FALSE
    query <- "SELECT Habitattype.Habitatcode AS Habitattype, Habitatsubtype.Habitatcode_subtype AS Habitatsubtype, Versie.VersieLSVI
    FROM (((Habitatsubtype INNER JOIN Habitattype ON Habitatsubtype.HabitattypeID = Habitattype.Id)
              INNER JOIN Habitatgroep ON Habitattype.HabitatgroepID = Habitatgroep.Id)
              INNER JOIN Indicator_habitat ON Habitatsubtype.Id = Indicator_habitat.HabitatsubtypeID)
              INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.ID"
    
    if(Versie[1] != "alle"){
      query <- sprintf("%s WHERE Versie.VersieLSVI = '%s'", query, Versie)
      Parametervoorwaarde <- TRUE
    }
    
    if(Habitatsubtype[1] != "alle"){
      if(Parametervoorwaarde){
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
      }
      query <- sprintf("%s %s Habitatsubtype.Habitatcode_subtype = '%s'", query, Voegwoord, Habitatsubtype)
    } else {
      if(Habitattype[1] != "alle"){
        if(Parametervoorwaarde){
          Voegwoord <- "AND"
        } else {
          Voegwoord <- "WHERE"
        }
        query <- sprintf("%s %s Habitattype.Habitatcode = '%s'", query, Voegwoord, Habitattype)
      } else {
        if(Habitatgroep[1] != "alle"){
          if(Parametervoorwaarde){
            Voegwoord <- "AND"
          } else {
            Voegwoord <- "WHERE"
          }
          query <- sprintf("%s %s Habitatgroep.Habitatgroepnaam = '%s'", query, Voegwoord, Habitatgroep)
        }
      }
    }

    connectie <- connecteerMetLSVIdb()
    Habitattypes <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    for(versie in unique(Habitattypes$VersieLSVI)){
      for(habitatsubtype in unique(as.character(Habitattypes$Habitatsubtype))){
        render(system.file("Habitatfiche.Rmd", package = "LSVI"), 
               params = list(Versie = versie, Habitatsubtype = habitatsubtype),
               output_file = sprintf("Habitatfiche_%s_%s.html",
                                     habitatsubtype,
                                     sub(versie, 
                                         pattern = " ", 
                                         replacement = "")),
               output_dir = getwd())
      }
    }
      
    
  }

