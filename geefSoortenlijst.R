#'
#'@title Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#'@description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de opgegeven parameters, zoals ze vermeld zijn in de habitatfiches.  In feite genereert ze een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam waarin de gespecificeerde parameters uitgeselecteerd zijn en waar voor andere parameters alle waarden uit de databank weergegeven zijn.  
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
#'@return tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten).  WetNaam is de volledige Latijnse naam met auteursnaam, WetNaamKort enkel genusnaam en soortnaam (zonder auteursnaam).
#'
#'@importFrom dplyr %>% select_ distinct_ filter group_by_ summarise_ ungroup bind_rows mutate_ right_join
#'@importFrom RODBC sqlQuery odbcClose
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

    connectie <- connecteerMetLSVIdb()
    Selectiegegevens <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    #nu de soortgegevens ophalen:
    #eerst oplijsten welke gegevens moeten opgehaald worden per niveau van Soortengroep en SoortengroepSoort
    SoortengroepIDperNiveau <- Selectiegegevens %>%
      select_(~SoortengroepID, ~NiveauSoortenlijstFiche) %>%
      distinct_() %>%
      filter(!is.na(SoortengroepID)) %>%
      group_by_(~NiveauSoortenlijstFiche) %>%
      summarise_(SoortengroepIDs = ~ paste(SoortengroepID, collapse=",")) %>%
      ungroup()
    
    #dan voor elk niveau de gegevens ophalen op basis van een query samengesteld op basis van het niveau, en deze gegevens aan elkaar plakken
    Soortenlijst <- NULL
    for(n in SoortengroepIDperNiveau$NiveauSoortenlijstFiche){
      ExtraOmschrijving <- ""
      ExtraJointabellen_begin <- ""
      ExtraJointabellen_eind <- ""
      if(n > 1){
        for(i in 2:n){
          ExtraOmschrijving <- sprintf("%s Soortengroep%s.Omschrijving AS Omschrijving%s,", ExtraOmschrijving, i, i)
          ExtraJointabellen_begin <- ifelse(nchar(ExtraJointabellen_begin)>20,
                                            paste(substr(ExtraJointabellen_begin, 1, 12),"(",
                                                  substr(ExtraJointabellen_begin, 13, nchar(ExtraJointabellen_begin)), collapse = ""),
                                            ExtraJointabellen_begin)
          ExtraJointabellen_begin <- sprintf("%s INNER JOIN (Soortengroep as Soortengroep%s INNER JOIN SoortengroepSoort as SoortengroepSoort%s ON Soortengroep%s.Id = SoortengroepSoort%s.SoortengroepID)", 
                                             ExtraJointabellen_begin, i, i, i, i)
          ExtraJointabellen_eind <- sprintf(" ON SoortengroepSoort%s.SoortensubgroepID = Soortengroep%s.Id)%s", 
                                            ifelse(i-1==1,"",i-1), i, ExtraJointabellen_eind)
        }
        ExtraJointabellen <- paste(ExtraJointabellen_begin, ExtraJointabellen_eind, collapse = "")
      } else {
        ExtraJointabellen <- ")"
      }
  
      query_soortenlijst <- sprintf("SELECT Soortengroep.Id as SoortengroepID, Soortengroep.Omschrijving,%s Soort.WetNaam, Soort.NedNaam,
                                            Soortensubgroep.Naam AS NedNaam_groep, Soortensubgroep.WetNaam AS WetNaam_groep
                                       FROM (((Soortengroep INNER JOIN SoortengroepSoort ON Soortengroep.Id = SoortengroepSoort.SoortengroepID)%s
                                              LEFT JOIN Soort ON SoortengroepSoort%s.SoortID = Soort.Id)
                                              LEFT JOIN Soortengroep as Soortensubgroep ON SoortengroepSoort%s.SoortensubgroepID = Soortensubgroep.Id
                                       WHERE Soortengroep.Id in (%s)",
                                       ExtraOmschrijving, ExtraJointabellen, ifelse(n==1,"",n), ifelse(n==1,"",n),
                                       SoortengroepIDperNiveau[SoortengroepIDperNiveau$NiveauSoortenlijstFiche==n,"SoortengroepIDs"])

      connectie <- connecteerMetLSVIdb()
      Soortenlijst_n <- sqlQuery(connectie, query_soortenlijst, stringsAsFactors = FALSE)
      odbcClose(connectie)
      
      Soortenlijst <- Soortenlijst %>%
        bind_rows(Soortenlijst_n)
    }
    
    #Kolommen met WetNaam (uit tabellen Soortengroep en Soort) samenvoegen, id voor NedNaam, en een kolom WetNaamKort toevoegen
    Soortenlijst <- Soortenlijst %>%
      mutate_(
        WetNaam = ~ifelse(is.na(WetNaam), WetNaam_groep, WetNaam),
        WetNaamKort = ~
          gsub(
            pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
            replacement = "\\1 \\2",
            x = WetNaam
          ),
        NedNaam = ~ifelse(is.na(NedNaam), NedNaam_groep, NedNaam),
        WetNaam_groep = ~NULL,
        NedNaam_groep = ~NULL
      ) 
    
    #kolommen wissen die enkel NA's bevatten
    Soortenlijst <- Filter(function(x)!all(is.na(x)),Soortenlijst)
    
    #soortgegevens aan selectiegegevens plakken
    SoortenlijstSelectie <- Selectiegegevens %>%
      left_join(Soortenlijst, by = ("SoortengroepID" = "SoortengroepID")) %>%
      mutate_(
        NiveauSoortenlijstFiche = ~NULL
      )
    
    return(SoortenlijstSelectie)  
  }

