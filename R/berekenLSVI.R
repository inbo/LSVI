#' @title Berekent de LSVI op basis van de opgegeven opnames
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding van het gevraagde habitatsubtype op basis van de opgegeven opnames.  
#'
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening gemaakt wordt, bv. "Versie 2" of "Versie 3".  Bij de default "alle" wordt de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de streefwaarde.  De betekenissen van de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de tabel Versie in de databank en kan opgevraagd met de functie geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk kwaliteitsniveau de berekening gemaakt moet worden (strikt genomen is de berekening van de LSVI de berekening volgens kwaliteitsniveau 1).
#' 
#' @inheritParams connecteerMetLSVIdb
#'
#' @return Deze functie genereert de resultaten in de vorm van een tabel met de meetresultaten en scores per indicator.
#' 
#' @examples 
#' berekenLSVI(Versie = "Versie 3", Kwaliteitsniveau = "1")
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#'
#'
berekenLSVI <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"),
           Kwaliteitsniveau = c("alle", "1", "2"),
           Server = "inbosql03\\prd",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "D0122_AppR",
           Wachtwoord = "19D939F1-BCCE-439F-9ED4-6A886E038A6D"){
    match.arg(Versie)
    match.arg(Kwaliteitsniveau)
    
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
    
    connectie <- connecteerMetLSVIdb(Server, Databank, Gebruiker, Wachtwoord)
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
    
    connectie <- connecteerMetLSVIdb(Server, Databank, Gebruiker, Wachtwoord)
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
    
    
    connectie <- connecteerMetLSVIdb(Server, Databank, Gebruiker, Wachtwoord)
    Voorwaarden <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)
    
    #Voorwaarden mergen met de nog in te voeren tabel (analoog aan Data_indicatoren in berekenLSVIgooglesheet, evt. hier aparte functie aanroepen die de berekening zelf doet op basis van VoorwaardeID's?)
    
  }
