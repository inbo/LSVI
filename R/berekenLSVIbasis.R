#' @title Berekent de LSVI op basis van VoorwaardeID en opgegeven waarden
#'
#' @description Deze hulpfunctie bepaalt de Lokale Staat van Instandhouding op basis van een opgegeven tabel met VoorwaardeID en een opgegeven waarde (die in het juiste formaat moet zijn).  Ze is bedoeld als technische hulpfunctie, een gelijkaardige functie met meer mogelijkheden voor invoer is berekenLSVI.
#'
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening gemaakt wordt, bv. "Versie 2" of "Versie 3".  Bij de default "alle" wordt de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de streefwaarde.  De betekenissen van de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de tabel Versie in de databank en kan opgevraagd met de functie geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk kwaliteitsniveau de berekening gemaakt moet worden (strikt genomen is de berekening van de LSVI de berekening volgens kwaliteitsniveau 1).
#' @param Data_voorwaarden Gegevens over de opgemeten indicatoren in de vorm van een data.frame met velden ID, Habitatsubtype, VoorwaardeID en Waarde, waarbij ID een groeperende variabele is voor een opname (plaats en tijdstip) en Waarde de waarde die voor die indicator geobserveerd of gemeten is.  Het type van deze waarde moet overeenkomen met het type dat verwacht wordt volgens de LSVI (geheel getal als een aantal (soorten) verwacht wordt, decimaal getal tussen 0 en 100 als een percentage verwacht wordt, een van de mogelijke categorieen bij een categorische variabele,...)  VoorwaardeID komt overeen met de ID in de databank die gekoppeld is aan de voorwaarde en Habitatsubtype moet overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door geefUniekeWaarden("Habitatsubtype", "Habitatcode_subtype")).
#' 
#'
#' @return Deze functie genereert de resultaten in de vorm van een tabel met de meetresultaten en scores per indicator.
#' 
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% left_join summarise_ select_ mutate_ group_by_ ungroup filter_ bind_rows distinct_ full_join
#' @importFrom assertthat assert_that has_name
#' @importFrom pander evals
#'
#'
berekenLSVIbasis <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"),
           Kwaliteitsniveau = c("alle", "1", "2"),
           Data_voorwaarden){
    
    #controle invoer
    match.arg(Versie)
    Kwaliteitsniveau <- ifelse(Kwaliteitsniveau==1, "1", 
                               ifelse(Kwaliteitsniveau==2, "2", 
                                      Kwaliteitsniveau))
    match.arg(Kwaliteitsniveau)
    assert_that(inherits(Data_voorwaarden, "data.frame"))
    assert_that(has_name(Data_voorwaarden, "ID"))
    assert_that(has_name(Data_voorwaarden, "VoorwaardeID"))
    if(!all(Data_voorwaarden$VoorwaardeID %in% geefUniekeWaarden("Voorwaarde", "Id"))){
      stop("Niet alle waarden vermeld onder Data_voorwaarden$VoorwaardeID komen overeen met waarden vermeld in de databank.")
    }
    assert_that(has_name(Data_voorwaarden, "Waarde"))
    
    #nodige info ophalen uit de databank
    Voorwaarden <- 
      ifelse(Versie[1]=="alle",
             ifelse(Kwaliteitsniveau[1]=="alle","",
                    sprintf("WHERE Beoordeling.Kwaliteitsniveau = '%s'", Kwaliteitsniveau[1])),
             ifelse(Kwaliteitsniveau[1]=="alle",
                    sprintf("WHERE Versie.VersieLSVI = '%s'", Versie[1]),
                    sprintf("WHERE Versie.VersieLSVI = '%s' AND Beoordeling.Kwaliteitsniveau = '%s'", Versie[1], Kwaliteitsniveau[1])))
    
    #in hoeverre is het naar performantie toe zinvol om enkel de te onderzoeken habitattypes te selecteren?
    #dit dan hier doen door een AND toe te voegen als de Voorwaarden-string niet "" is
    
    
    query <- sprintf(
      "SELECT 
      Versie.VersieLSVI,
      Habitatsubtype.Habitatcode_subtype,
      Criterium.Naam AS Criterium,
      Indicator.Naam AS Indicator,
      Beoordeling.Id AS BeoordelingID,
      Beoordeling.Kwaliteitsniveau,
      Beoordeling.Beoordeling_letterlijk
      FROM (((((Beoordeling INNER JOIN Indicator_beoordeling 
      ON Beoordeling.Indicator_beoordelingID = Indicator_beoordeling.Id)
      INNER JOIN (Indicator INNER JOIN Criterium ON Indicator.CriteriumID = Criterium.Id)
      ON Indicator_beoordeling.IndicatorID = Indicator.Id)
      INNER JOIN IndicatortabellenKoppeling 
      ON Indicator_beoordeling.ID = IndicatortabellenKoppeling.Indicator_beoordelingID)
      INNER JOIN Indicator_habitat 
      ON IndicatortabellenKoppeling.Indicator_habitatID = Indicator_habitat.Id)
      INNER JOIN Habitatsubtype ON Indicator_habitat.HabitatsubtypeID = Habitatsubtype.Id)
      INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.Id %s",
      Voorwaarden)
    
    connectie <- connecteerMetLSVIdb()
    GroeperendeInfo <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    BeoordelingIDs <- paste(unique(GroeperendeInfo$BeoordelingID), collapse = ",")
    
    query <- 
      sprintf("
              WITH voorwaardencombinatie
              AS
              (
              SELECT CombinerenVoorwaarden.Id,
              CombinerenVoorwaarden.BeoordelingID,
              CombinerenVoorwaarden.VoorwaardeID1,
              CombinerenVoorwaarden.VoorwaardeID2,
              CombinerenVoorwaarden.ChildID1,
              CombinerenVoorwaarden.ChildID2,
              CombinerenVoorwaarden.BewerkingAND
              FROM CombinerenVoorwaarden
              WHERE CombinerenVoorwaarden.BeoordelingID in (%s)
              UNION ALL
              SELECT CombinerenVoorwaarden2.Id,
              CombinerenVoorwaarden2.BeoordelingID,
              CombinerenVoorwaarden2.VoorwaardeID1,
              CombinerenVoorwaarden2.VoorwaardeID2,
              CombinerenVoorwaarden2.ChildID1,
              CombinerenVoorwaarden2.ChildID2,
              CombinerenVoorwaarden2.BewerkingAND
              FROM CombinerenVoorwaarden AS CombinerenVoorwaarden2
              INNER JOIN CombinerenVoorwaarden
              ON CombinerenVoorwaarden2.Id = CombinerenVoorwaarden.ChildID1
              UNION ALL
              SELECT CombinerenVoorwaarden3.Id,
              CombinerenVoorwaarden3.BeoordelingID,
              CombinerenVoorwaarden3.VoorwaardeID1,
              CombinerenVoorwaarden3.VoorwaardeID2,
              CombinerenVoorwaarden3.ChildID1,
              CombinerenVoorwaarden3.ChildID2,
              CombinerenVoorwaarden3.BewerkingAND
              FROM CombinerenVoorwaarden AS CombinerenVoorwaarden3
              INNER JOIN CombinerenVoorwaarden
              ON CombinerenVoorwaarden3.Id = CombinerenVoorwaarden.ChildID2
              )
              Select * FROM voorwaardencombinatie",
              BeoordelingIDs)
    
    connectie <- connecteerMetLSVIdb()
    CombinatieVoorwaarden <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    
    VoorwaardeIDs <- 
      paste(unique(c(CombinatieVoorwaarden[!is.na(CombinatieVoorwaarden$VoorwaardeID1),]$VoorwaardeID1,
                     CombinatieVoorwaarden[!is.na(CombinatieVoorwaarden$VoorwaardeID2),]$VoorwaardeID2)), 
            collapse = ",")
    
    query <- 
      sprintf("
              SELECT Voorwaarde.Id AS VoorwaardeID,
              VoorwaardeNaam.Omschrijving AS VoorwaardeNaam,
              Voorwaarde.Referentiewaarde,
              Voorwaarde.Operator,
              Voorwaarde.SoortengroepID,
              AnalyseVariabele.VariabeleNaam,
              AnalyseVariabele.Eenheid,
              TypeVariabele.Naam AS TypeVariabele,
              Invoermasker.Waarde AS Invoermasker,
              Invoermasker.Volgnummer,
              Vegetatielaag.Omschrijving AS Vegetatielaag
              FROM (Voorwaarde
              LEFT JOIN VoorwaardeNaam ON Voorwaarde.VoorwaardeNaamID = VoorwaardeNaam.Id)
              INNER JOIN (((AnalyseVariabele LEFT JOIN TypeVariabele ON AnalyseVariabele.TypeVariabeleID = TypeVariabele.Id)
              LEFT JOIN Vegetatielaag ON AnalyseVariabele.VegetatielaagID = Vegetatielaag.Id)
              LEFT JOIN Invoermasker ON AnalyseVariabele.Id = Invoermasker.AnalyseVariabeleID)
              ON Voorwaarde.AnalyseVariabeleID = AnalyseVariabele.Id
              WHERE Voorwaarde.Id in (%s)",
              VoorwaardeIDs)
    
    
    connectie <- connecteerMetLSVIdb()
    Voorwaarden <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    #data koppelen aan voorwaarden uit de databank en dan 'berekeningen' (vgl met referentiewaarde) doen
    Data_voorwaarden <- Data_voorwaarden %>%
      left_join(Voorwaarden, by = c("VoorwaardeID" = "VoorwaardeID"))
    
    #berekeningen: vergelijking getallen met referentiewaarde
    Resultaat_getal <- Data_voorwaarden %>%
      filter_(~TypeVariabele %in% c("Geheel getal", "Decimaal getal", "Percentage"))
    
    #pipe onderbreken voor foutcontrole: test of getallen het juiste formaat hebben
    Foutcontrole <- Resultaat_getal %>%
      select_(~Waarde, ~TypeVariabele) %>%
      mutate_(
        WaardeGetal = ~as.numeric(Waarde),
        WaardeInt = ~as.integer(Waarde)
      )
    if(max(is.na(Foutcontrole$WaardeGetal) & !is.na(Foutcontrole$Waarde))){
      stop("Foute invoer in Data_voorwaarden$Waarde: geen getal ingevoerd waar een getal verwacht wordt")
    }
    if(max(!is.na(Foutcontrole$WaardeGetal) & Foutcontrole$WaardeGetal < 0)){
      stop("Foute invoer in Data_voorwaarden$Waarde: een negatief getal ingevoerd")  #nog checken in oude db of er refwaarden zijn die negatief mogen zijn
    }
    if(max(Foutcontrole$TypeVariabele == "Geheel getal" & !is.na(Foutcontrole$WaardeGetal) & Foutcontrole$WaardeInt != Foutcontrole$WaardeGetal)){
      stop("Foute invoer in Data_voorwaarden$Waarde: een kommagetal ingevoerd waar een geheel getal verwacht wordt")
    }
    if(max(Foutcontrole$TypeVariabele == "Percentage" & !is.na(Foutcontrole$WaardeGetal) & Foutcontrole$WaardeGetal > 100)){
      stop("Foute invoer in Data_voorwaarden$Waarde: een getal > 100 ingevoerd waar een percentage verwacht wordt")
    }
    
    #berekening verderzetten
    Resultaat_getal <- Resultaat_getal %>%
      mutate_(
        Vergelijking = ~paste(Waarde, Operator, Referentiewaarde, sep = " "),
        Status = ~ifelse(!is.na(Waarde),
                         sapply(evals(Vergelijking), function(x){as.logical(x[2])}),
                         NA),
        Vergelijking = ~NULL
      )
    
    
    #berekeningen: vergelijking categorieen met referentiewaarde
    Resultaat_categorie <- Data_voorwaarden %>%
      filter_(~TypeVariabele == "Categorie") %>%
      mutate_(
        WaardeN = ~ifelse(tolower(Waarde) == tolower(Invoermasker), Volgnummer, -1),
        RefWaardeN = ~ifelse(tolower(Referentiewaarde) == tolower(Invoermasker), Volgnummer, -1)
      ) %>%
      group_by_(~ID, ~VoorwaardeID, ~Waarde, ~Habitatsubtype, ~VoorwaardeNaam,
                ~Referentiewaarde, ~Operator, ~SoortengroepID, ~VariabeleNaam,
                ~Eenheid, ~TypeVariabele, ~Vegetatielaag) %>%
      summarise_(
        WaardeN = ~max(WaardeN),
        RefWaardeN = ~max(RefWaardeN)
      ) %>%
      ungroup()
    
    #pipe even onderbreken voor de foutcontrole
    if(all(!is.na(Resultaat_categorie$Waarde)) & min(Resultaat_categorie$WaardeN) < 0){
      stop("Foute invoer in Data_voorwaarden$Waarde: niet alle categorische waarden komen overeen met het invoermasker uit de databank")
    }
    
    #en de berekening verder afwerken
    Resultaat_categorie <- Resultaat_categorie %>%
      mutate_(
        Vergelijking = ~paste(WaardeN, Operator, RefWaardeN, sep = " "),
        Status = ~ifelse(!is.na(Waarde),
                         sapply(evals(Vergelijking), function(x){as.logical(x[2])}),
                         NA),
        Vergelijking = ~NULL,
        WaardeN = ~NULL,
        RefWaardeN = ~NULL
      ) 
    
    
    
    #berekeningen: nog uitwerken!
    Resultaat_janee <- Data_voorwaarden %>%
      filter_(~TypeVariabele == "Ja/nee") %>%
      mutate_(Status = NA)                #uitwerken zodra hier voorbeelden van in de db zitten
    
    #samenvoegen resultaten berekeningen
    Resultaat <- Resultaat_getal %>%
      bind_rows(Resultaat_categorie) %>%
      bind_rows(Resultaat_janee) %>%
      mutate_(
        Invoermasker = ~NULL,
        Volgnummer = ~NULL
      )
    
    #nu een recursieve functie om de voorwaarden te combineren tot een beoordeling
    groepeerVoorwaarden <- function(CombinerenVoorwaardenID){
      Record <- CombinatieVoorwaarden %>%
        filter_(~Id == CombinerenVoorwaardenID)
      Data <- data.frame(ID = NULL, VoorwaardeID = NULL, 
                         Beoordeling_indicator = NULL, BeoordelingID = NULL)
      if(!is.na(Record$ChildID1)){
        Data <- Data %>%
          bind_rows(groepeerVoorwaarden(Record$ChildID1))
      }
      if(!is.na(Record$ChildID2)){
        Data <- Data %>%
          bind_rows(groepeerVoorwaarden(Record$ChildID2))
      }
      if(!is.na(Record$VoorwaardeID1)){
        Data_resultaat <- Resultaat %>%
          filter_(~VoorwaardeID %in% Record$VoorwaardeID1) %>%
          select_(~ID, ~VoorwaardeID, ~Status) %>%
          mutate_(
            Beoordeling_indicator = ~Status,
            Status = ~NULL
          )
        Data <- Data %>%
          bind_rows(Data_resultaat)
      }
      if(!is.na(Record$VoorwaardeID2)){
        Data_resultaat <- Resultaat %>%
          filter_(~VoorwaardeID %in% Record$VoorwaardeID2) %>%
          select_(~ID, ~VoorwaardeID, ~Status) %>%
          mutate_(
            Beoordeling_indicator = ~Status,
            Status = ~NULL
          )
        Data <- Data %>%
          bind_rows(Data_resultaat)
      }
      
      Beoordelingberekening <- Data %>%
          group_by_(~ID) %>%
          summarise_(
            Beoordeling_indicator = 
              ~ifelse(all(!is.na(Beoordeling_indicator)),
                      ifelse(Record$BewerkingAND,
                             as.logical(min(Beoordeling_indicator)),
                             as.logical(max(Beoordeling_indicator))),
                      NA)
          ) %>%
          ungroup()
      Data <- Data %>%
        mutate_(
          Beoordeling_indicator = ~NULL
        ) %>% 
        left_join(Beoordelingberekening, by = c("ID" = "ID"))
      
      return(Data)
    }
    
    #recursieve functie uitvoeren voor alle beoordelingen en dan extra info aan hangen
    Data <- data.frame(ID = NULL, VoorwaardeID = NULL, 
                       Beoordeling_indicator = NULL, BeoordelingID = NULL)
    for(i in unique(CombinatieVoorwaarden$BeoordelingID)){
      Data <- Data %>%
        bind_rows(groepeerVoorwaarden((CombinatieVoorwaarden %>%
                                        filter_(~BeoordelingID == i))$Id) %>%
                    mutate_(BeoordelingID = ~i))
    }
    
    Resultaat_beoordeling <- Data %>%
      left_join(Resultaat,
                by = c("ID" = "ID", "VoorwaardeID" = "VoorwaardeID")) %>%
      full_join(GroeperendeInfo, 
                by = c("BeoordelingID" = "BeoordelingID", 
                       "Habitatsubtype" = "Habitatcode_subtype"))
    
    #resultaten op niveau van indicator uitselecteren
    Resultaat_indicator <- Resultaat_beoordeling %>%
      select_(~ID, ~Habitatsubtype, ~VersieLSVI, ~Criterium, ~Kwaliteitsniveau, 
              ~Indicator, ~Beoordeling_letterlijk, ~Beoordeling_indicator,
              ~BeoordelingID) %>%
      distinct_()
    
    #resultaten op niveau van criterium afleiden
    Resultaat_criterium <- Resultaat_beoordeling %>%
      group_by_(~ID, ~Habitatsubtype, ~VersieLSVI, ~Criterium, ~Kwaliteitsniveau) %>%
      summarise_(
        Beoordeling_criterium = ~as.logical(min(Beoordeling_indicator))
      ) %>%
      ungroup()
      
    
    return(list(as.data.frame(Resultaat_criterium), Resultaat_indicator, Resultaat_beoordeling))
  }
