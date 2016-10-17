#'
#'Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#'Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de opgegeven parameters, zoals ze vermeld zijn in de habitatfiches.  In feite genereert ze een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam en NedNaam waarin de gespecificeerde parameters uitgeselecteerd zijn en waar voor andere parameters alle waarden uit de databank weergegeven zijn.  
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Voor eenzelfde parameter twee of meer waarden opgeven kan door de waarden te scheiden door 'or' en het geheel tussen haakjes te zetten.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#'@param Versie De versie van het LSVI-rapport, bv. 'Versie 2' of 'Versie 3'.  Bij de default 'alle' worden de soortenlijsten voor de verschillende versies gegeven.
#'@param Habitatgroep Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen geselecteerd worden, bv. bossen, heides,...   en 'alle' (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type geselecteerd wordt.
#'@param Habitattype Parameter waarmee een habitattype kan geselecteerd worden.  Als dit habitattype meerdere subtypes heeft, zullen de soortenlijsten van alle subtypes van dit habitattype weergegeven worden.
#'@param Habitatsubtype Parameter waarmee een habitatsubtype geselecteerd kan worden.  Als deze waarde ingevuld is, is het niet nodig om de parameters Habitatgroep en Habitattype te specifiëren.
#'@param Criterium Het LSVI-criterium waarvoor de soortenlijst gegeven wordt: Vegetatie, Structuur, Verstoring of Alle.
#'@param Indicator De indicator waarvoor de soortenlijst gegeven wordt.
#'
#'@return tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten)
#'
#'
#'
geefSoortenlijst <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Habitatgroep","Habitatgroepnaam"),  
           Habitattype = geefUniekeWaarden("Habitattype","Habitatcode"), 
           Habitatsubtype = geefUniekeWaarden("Habitatsubtype","Habitatcode_subtype"), 
           Criterium = geefUniekeWaarden("Criterium","Naam"), 
           Indicator = geefUniekeWaarden("Indicator","Naam")){
    match.arg(Versie)
    match.arg(Habitatgroep)
    match.arg(Habitattype)
    match.arg(Habitatsubtype)
    match.arg(Criterium)
    match.arg(Indicator)
    Parametervoorwaarde <- FALSE
    query <- "SELECT Versie.VersieLSVI, Habitattype.Habitatcode AS Habitattype, Habitatsubtype.Habitatcode_subtype AS Habitatsubtype,
                Criterium.Naam AS Criterium, Indicator.Naam AS Indicator, 
                Indicator_habitat.SoortengroepID, Indicator_habitat.NiveauSoortenlijstFiche
              FROM ((Indicator_habitat 
                        INNER JOIN ((Habitatsubtype INNER JOIN Habitattype ON Habitatsubtype.HabitattypeID = Habitattype.Id)
                                INNER JOIN Habitatgroep ON Habitattype.HabitatgroepID = Habitatgroep.Id)
                        ON Indicator_habitat.HabitatsubtypeID = Habitatsubtype.Id)
                      INNER JOIN (Indicator INNER JOIN Criterium ON Indicator.CriteriumID = Criterium.Id)
                      ON Indicator_habitat.IndicatorID = Indicator.Id)
                    INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.Id"
    if(Versie[1] != "alle"){
      query <- sprintf("%s WHERE Versie.VersieLSVI = '%s'", query, Versie)
      Parametervoorwaarde <- TRUE
    }
    if(Habitatgroep[1] != "alle"){
      if(Parametervoorwaarde){
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <- sprintf("%s %s Habitatgroep.Habitatgroepnaam = '%s'", query, Voegwoord, Habitatgroep)
    }
    if(Habitattype[1] != "alle"){
      if(Parametervoorwaarde){
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <- sprintf("%s %s Habitattype.Habitatcode = '%s'", query, Voegwoord, Habitattype)
    }
    if(Habitatsubtype[1] != "alle"){
      if(Parametervoorwaarde){
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <- sprintf("%s %s Habitatsubtype.Habitatcode_subtype = '%s'", query, Voegwoord, Habitatsubtype)
    }
    if(Criterium[1] != "alle"){
      if(Parametervoorwaarde){
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <- sprintf("%s %s Criterium.Naam = '%s'", query, Voegwoord, Criterium)
    }
    if(Indicator[1] != "alle"){
      if(Parametervoorwaarde){
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <- sprintf("%s %s Indicator.Naam = '%s'", query, Voegwoord, Indicator)
    }
    Selectiegegevens <- connecteerMetLSVIdb(query)
  }

