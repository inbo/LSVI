#'
#'Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#'Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de opgegeven parameters, zoals ze vermeld zijn in de habitatfiches.  In feite genereert ze een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, WetNaam en NedNaam waarin de gespecificeerde parameters uitgeselecteerd zijn en waar voor andere parameters alle waarden uit de databank weergegeven zijn.  
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Voor eenzelfde parameter twee of meer waarden opgeven kan door de waarden te scheiden door 'or' en het geheel tussen haakjes te zetten.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#'@param Versie De versie van het LSVI-rapport, bv. 'Versie 2' of 'Versie 3'.  Bij de default 'alle' worden de soortenlijsten voor de verschillende versies gegeven.
#'@param Habitatgroep Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen geselecteerd worden, bv. bossen, heides,...   en 'alle' (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type geselecteerd wordt.
#'@param Habitattype Parameter waarmee een habitattype kan geselecteerd worden.  Als dit habitattype meerdere subtypes heeft, zullen de soortenlijsten van alle subtypes van dit habitattype weergegeven worden.
#'@param Habitatsubtype Parameter waarmee een habitatsubtype geselecteerd kan worden.  Als deze waarde ingevuld is, is het niet nodig om de parameters Habitatgroep en Habitattype te specifiëren.
#'@param Criterium ...
#'@param Indicator ...
#'
#'@return tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, WetNaam en NedNaam
#'
#'
#'
geefSoortenlijst <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Versie","VersieLSVI"),      #waarden tabelnaam en veldnaam nog aanpassen!!! (en beschrijving nog aanvullen)
           Habitattype = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatsubtype = geefUniekeWaarden("Versie","VersieLSVI"), 
           Criterium = geefUniekeWaarden("Versie","VersieLSVI"), 
           Indicator = geefUniekeWaarden("Versie","VersieLSVI")){
    match.arg(Versie)
    query <- "SELECT"
  }

getVersions <- function(){c("alle", "2016")}