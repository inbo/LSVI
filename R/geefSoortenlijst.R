#' @title Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de opgegeven parameters.  In feite genereert ze een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam waarin de gespecificeerde parameters uitgeselecteerd zijn en waar voor andere parameters alle waarden uit de databank weergegeven zijn.  
#' 
#' Er zijn 2 opties: de soorten weergeven zoals in de habitatfiches (op soortniveau, genusniveau of hoger niveau, zoals het in de habitatfiches vermeld is) of alle soorten op soortniveau weergeven en dus bij soortengroepen alle mogelijke soorten van deze groep weergeven.  Deze opties kunnen opgegeven worden in de parameter Soortenlijsttype.
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#' @param Versie De versie van het LSVI-rapport, bv. "Versie 2" of "Versie 3".  Bij de default "alle" worden de soortenlijsten voor de verschillende versies gegeven.
#' @param Habitatgroep Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen geselecteerd worden, bv. "Bossen", "Heiden", "(Half-)natuurlijke graslanden", "Zoete wateren",...   en "alle" (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type geselecteerd wordt.
#' @param Habitattype Parameter waarmee een habitattype kan geselecteerd worden.  Als dit habitattype meerdere subtypes heeft, zullen de soortenlijsten van alle subtypes van dit habitattype weergegeven worden.
#' @param Habitatsubtype Parameter waarmee een habitatsubtype geselecteerd kan worden.  Als deze waarde ingevuld is, is het niet nodig om de parameters Habitatgroep en Habitattype te specifieren.
#' @param Criterium Het LSVI-criterium waarvoor de soortenlijst gegeven wordt: "Vegetatie", "Structuur", "Verstoring" of "alle".
#' @param Indicator De indicator waarvoor de soortenlijst gegeven wordt.
#' @param Soortenlijsttype "LSVIfiche" betekent dat de soortenlijst van de habitatfiche wordt overgenomen, "Soortniveau" betekent dat alle soorten worden weergegeven die in de groepen vallen die aan de parameters voldoen (bv. alle soorten bomen en struiken als dit in LSVI-fiche vermeld is), "alle" betekent dat alle soorten en alle taxonomische en morfologische groepen worden weergegeven die volledig in de groepen vallen die aan de parameters voldoen (dus gelijkaardig als Soortniveau, maar dan uitgebreid naar hogere taxonomische en morfologische groepen).
#' 
#' @inheritParams connecteerMetLSVIdb
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#' 
#' @examples
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "LSVIfiche")
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "Soortniveau")
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "alle")
#'
#' @export
#'
#' @importFrom dplyr %>% select_ distinct_ filter group_by_ summarise_ ungroup mutate_ left_join rename_
#' @importFrom RODBC sqlQuery odbcClose
#'
#'
geefSoortenlijst <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Habitatgroep","Habitatgroepnaam"),  
           Habitattype = geefUniekeWaarden("Habitattype","Habitatcode"), 
           Habitatsubtype = geefUniekeWaarden("Habitatsubtype","Habitatcode_subtype"), 
           Criterium = geefUniekeWaarden("Criterium","Naam"), 
           Indicator = geefUniekeWaarden("Indicator","Naam"),
           Soortenlijsttype = c("LSVIfiche", "Soortniveau", "alle"),
           Server = "inbosql03\\prd",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "D0122_AppR",
           Wachtwoord = "19D939F1-BCCE-439F-9ED4-6A886E038A6D"){
    match.arg(Versie)
    match.arg(Habitatgroep)
    match.arg(Habitattype)
    match.arg(Habitatsubtype)
    match.arg(Criterium)
    match.arg(Indicator)
    match.arg(Soortenlijsttype)
    
    
    #eerst de selectiegegevens ophalen en de nodige gegevens uit tabel Indicator_habitat, query samenstellen op basis van parameters
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
    
    connectie <- connecteerMetLSVIdb(Server, Databank, Gebruiker, Wachtwoord)
    Selectiegegevens <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    #nu de soortgegevens ophalen:
    if(Soortenlijsttype[1] == "LSVIfiche"){
      #eerst oplijsten welke gegevens moeten opgehaald worden per niveau van Soortengroep en SoortengroepSoort
      SoortengroepIDperNiveau <- Selectiegegevens %>%
        select_(~SoortengroepID, ~NiveauSoortenlijstFiche) %>%
        distinct_() %>%
        filter(!is.na(SoortengroepID)) %>%
        group_by_(~NiveauSoortenlijstFiche) %>%
        summarise_(SoortengroepIDs = ~ paste(SoortengroepID, collapse=",")) %>%
        ungroup() %>%
        rename_(Niveau = ~NiveauSoortenlijstFiche)
      
      #dan voor elk niveau de gegevens ophalen
      Soortenlijst <- geefSoortenlijstInvoerniveau(SoortengroepIDperNiveau,
                                                   Server, Databank, Gebruiker, Wachtwoord)
      
    } else if(Soortenlijsttype == "Soortniveau" | Soortenlijsttype == "alle"){
      #de andere optie: gegevens van het diepste niveau ophalen
      SoortengroepIDs <- Selectiegegevens %>%
        select_(~SoortengroepID) %>%
        distinct_() %>%
        filter(!is.na(SoortengroepID)) %>%
        summarise_(SoortengroepIDs = ~ paste(SoortengroepID, collapse=","))
      
      Soortenlijst <- 
        geefSoortenlijstSoortniveau(Soortengroeplijst = SoortengroepIDs$SoortengroepIDs,
                                    Soortenlijsttype = Soortenlijsttype,
                                    Server, Databank, Gebruiker, Wachtwoord)
      
    } 
    
    
    #soortgegevens aan selectiegegevens plakken
    SoortenlijstSelectie <- Selectiegegevens %>%
      left_join(Soortenlijst, by = ("SoortengroepID" = "SoortengroepID")) %>%
      mutate_(
        NiveauSoortenlijstFiche = ~NULL
      )
    
    return(SoortenlijstSelectie)  
    
  }

