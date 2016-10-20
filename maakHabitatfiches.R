#'
#'Genereert habitatfiche(s) van LSVI op basis van de opgegeven parameters
#'
#'Deze functie genereert habitatfiches die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de habitatsubtypes die voldoen aan de opgegeven parameters.  
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Voor eenzelfde parameter twee of meer waarden opgeven kan door de waarden te scheiden door 'or' en het geheel tussen haakjes te zetten.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#'@param Versie De versie van het LSVI-rapport, bv. 'Versie 2' of 'Versie 3'.  Bij de default 'alle' worden de soortenlijsten voor de verschillende versies gegeven.
#'@param Habitatgroep Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen geselecteerd worden, bv. Bossen, Heiden, (Half-)natuurlijke graslanden, Zoete wateren,...   en 'alle' (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type geselecteerd wordt.
#'@param Habitattype Parameter waarmee een habitattype kan geselecteerd worden.  Als dit habitattype meerdere subtypes heeft, zullen de soortenlijsten van alle subtypes van dit habitattype weergegeven worden.
#'@param Habitatsubtype Parameter waarmee een habitatsubtype geselecteerd kan worden.  Als deze waarde ingevuld is, is het niet nodig om de parameters Habitatgroep en Habitattype te specifiëren.
#'
#'@return Habitatfiches in de vorm van html-files die in de workspace opgeslagen worden.
#'
#'@importFrom rmarkdown render
#'
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

    Habitattypes <- connecteerMetLSVIdb(query)
    
    for(versie in unique(Habitattypes$VersieLSVI)){
      for(habitatsubtype in unique(as.character(Habitattypes$Habitatsubtype))){
        render("Habitatfiche.Rmd", params = list(Versie = versie, Habitatsubtype = habitatsubtype))
      }
    }
      
    
  }

