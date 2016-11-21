#' @title Berekent de LSVI op basis van VoorwaardeID en opgegeven waarden
#'
#' @description Deze hulpfunctie bepaalt de Lokale Staat van Instandhouding op basis van een opgegeven tabel met VoorwaardeID en een opgegeven waarde (die in het juiste formaat moet zijn).  Ze is bedoeld als technische hulpfunctie, een gelijkaardige functie met meer mogelijkheden voor invoer is berekenLSVI.
#'
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening gemaakt wordt, bv. "Versie 2" of "Versie 3".  Bij de default "alle" wordt de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de streefwaarde.  De betekenissen van de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de tabel Versie in de databank en kan opgevraagd met de functie geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk kwaliteitsniveau de berekening gemaakt moet worden (strikt genomen is de berekening van de LSVI de berekening volgens kwaliteitsniveau 1).
#' @param Data_voorwaarden Gegevens over de opgemeten indicatoren in de vorm van een data.frame met velden ID, Habitatsubtype, VoorwaardeID en Waarde, waarbij ID een groeperende variabele is voor een opname (plaats en tijdstip) en Waarde de waarde die voor die indicator geobserveerd of gemeten is.  VoorwaardeID komt overeen met de ID in de databank die gekoppeld is aan de voorwaarde en Habitatsubtype moet overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door geefUniekeWaarden("Habitatsubtype", "Habitatcode_subtype")).
#' 
#'
#' @return Deze functie genereert de resultaten in de vorm van een tabel met de meetresultaten en scores per indicator.
#' 
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% left_join summarise_ select_ mutate_ group_by_ ungroup
#' @importFrom assertthat assert_that has_name
#' @importFrom pander evals
#'
#'
berekenLSVIbasis <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"),
           Kwaliteitsniveau = c("alle", "1", "2"),
           Data_voorwaarden){
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
              SELECT CombinerenVoorwaarden.BeoordelingID,
              CombinerenVoorwaarden.VoorwaardeID1,
              CombinerenVoorwaarden.VoorwaardeID2,
              CombinerenVoorwaarden.childID1,
              CombinerenVoorwaarden.childID2,
              CombinerenVoorwaarden.BewerkingAND
              FROM CombinerenVoorwaarden
              WHERE CombinerenVoorwaarden.BeoordelingID in (%s)
              UNION ALL
              SELECT CombinerenVoorwaarden2.BeoordelingID,
              CombinerenVoorwaarden2.VoorwaardeID1,
              CombinerenVoorwaarden2.VoorwaardeID2,
              CombinerenVoorwaarden2.childID1,
              CombinerenVoorwaarden2.childID2,
              CombinerenVoorwaarden2.BewerkingAND
              FROM CombinerenVoorwaarden AS CombinerenVoorwaarden2
              INNER JOIN CombinerenVoorwaarden
              ON CombinerenVoorwaarden2.Id = CombinerenVoorwaarden.childID1
              UNION ALL
              SELECT CombinerenVoorwaarden3.BeoordelingID,
              CombinerenVoorwaarden3.VoorwaardeID1,
              CombinerenVoorwaarden3.VoorwaardeID2,
              CombinerenVoorwaarden3.childID1,
              CombinerenVoorwaarden3.childID2,
              CombinerenVoorwaarden3.BewerkingAND
              FROM CombinerenVoorwaarden AS CombinerenVoorwaarden3
              INNER JOIN CombinerenVoorwaarden
              ON CombinerenVoorwaarden3.Id = CombinerenVoorwaarden.childID2
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
              AnalyseVariabele.Invoermasker,
              Vegetatielaag.Omschrijving AS Vegetatielaag
              FROM (Voorwaarde
              LEFT JOIN VoorwaardeNaam ON Voorwaarde.VoorwaardeNaamID = VoorwaardeNaam.Id)
              INNER JOIN ((AnalyseVariabele LEFT JOIN TypeVariabele ON AnalyseVariabele.TypeVariabeleID = TypeVariabele.Id)
              LEFT JOIN Vegetatielaag ON AnalyseVariabele.VegetatielaagID = Vegetatielaag.Id)
              ON Voorwaarde.AnalysevariabeleID = AnalyseVariabele.Id
              WHERE Voorwaarde.Id in (%s)",
              VoorwaardeIDs)
    
    
    connectie <- connecteerMetLSVIdb()
    Voorwaarden <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    Data_voorwaarden <- Data_voorwaarden %>%
      left_join(Voorwaarden, by = c("VoorwaardeID" = "VoorwaardeID"))
    
    #hier, of in de volgende subdata, moet getest worden of Waarde het juiste formaat heeft
    #(aangegeven in TypeVariabele)
    #of is de boodschap bij een fout dataformaat voldoende duidelijk zonder deze controle?
    
    
    Resultaat_getal <- Data_voorwaarden %>%
      filter_(~TypeVariabele %in% c("Geheel getal", "Decimaal getal", "Percentage")) %>%
      mutate_(
        Vergelijking = ~paste(Waarde, Operator, Referentiewaarde, sep = " "),
        Status = ~sapply(evals(Vergelijking), function(x){as.logical(x[2])}),
        Vergelijking = ~NULL
      )
    
    Resultaat_categorie <- Data_voorwaarden %>%
      filter_(~TypeVariabele == "Categorie") %>%
      mutate_(
        WaardeF = ~factor(Waarde, 
                          levels = c("afwezig","sporadisch", "zeldzaam", "occasioneel", "frequent", "abundant", "codominant", "dominant"),
                          ordered = is.ordered(levels)),
        RefWaardeF = ~factor(Referentiewaarde, 
                          levels = c("afwezig","sporadisch", "zeldzaam", "occasioneel", "frequent", "abundant", "codominant", "dominant"),
                          ordered = is.ordered(levels)),
        WaardeN = ~as.numeric(WaardeF),
        RefWaardeN = ~as.numeric(RefWaardeF),
        Vergelijking = ~paste(WaardeN, Operator, RefWaardeN, sep = " "),
        Status = ~sapply(evals(Vergelijking), function(x){as.logical(x[2])})
      ) %>%
      select_(
        ~ID, ~VoorwaardeID, ~Waarde, ~Habitatsubtype, ~VoorwaardeNaam,
        ~Referentiewaarde, ~Operator, ~SoortengroepID, ~VariabeleNaam,
        ~Eenheid, ~Invoermasker, ~Vegetatielaag, ~Status
      )
    # Levels <- Resultaat_categorie %>%
    #   select_(~VariabeleNaam, ~Invoermasker) %>%
    #   distinct_()
    # for(i in 1:nrow(Levels)){
    #   levels$VariabeleName[i] <- unlist(strsplit(Levels$Invoermasker[i], ","))
    # }
    #Dit werkt niet, beste is om de databankstructuur aan te passen en hier met een subtabel Invoermasker te werken
    #die verwijst naar AnalyseVariabeleID en de velden Volgorde en Level heeft
    #voorlopig even opgelost door manueel de boel toe te voegen
    
      
    Resultaat_janee <- Data_voorwaarden %>%
      filter_(~TypeVariabele == "Ja/nee") %>%
      mutate_(Status = NA)                #uitwerken zodra hier voorbeelden van in de db zitten
    
    Resultaat <- Resultaat_getal %>%
      bind_rows(Resultaat_categorie) %>%
      bind_rows(Resultaat_janee)
    
    #in onderstaande moet ook nog rekening gehouden worden met childID1 en childID2
    Resultaat_beoordeling <- CombinatieVoorwaarden %>%
      left_join(Resultaat, 
                by = c("VoorwaardeID1" = "VoorwaardeID"),
                suffix = c(".0",".1")) %>%
      left_join(Resultaat, 
                by = c("VoorwaardeID2" = "VoorwaardeID", "ID" = "ID", "Habitatsubtype" = "Habitatsubtype"),
                suffix = c(".1",".2")) %>%
      mutate_(
        Beoordeling_indicator = ~ifelse(BewerkingAND,
                                        Status.1 & Status.2,
                                        ifelse(!is.na(Status.2),
                                               Status.1 | Status.2,
                                               Status.1))
      ) %>%
      left_join(GroeperendeInfo, by = c("BeoordelingID" = "BeoordelingID", "Habitatsubtype" = "Habitatcode_subtype"))
    
    Resultaat_criterium <- Resultaat_beoordeling %>%
      group_by_(~ID, ~Habitatsubtype, ~VersieLSVI, ~Criterium, ~Kwaliteitsniveau) %>%
      summarise_(
        Beoordeling_criterium = ~(!FALSE %in% Beoordeling_indicator)
      ) %>%
      ungroup()
      
    
    return(list(Resultaat_beoordeling, Resultaat_criterium))
  }
