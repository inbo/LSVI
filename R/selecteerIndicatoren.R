#' @title Selecteert indicatoren LSVI op basis van de opgegeven parameters
#'
#' @description Deze hulpfunctie selecteert de indicatoren die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding voor de opgegeven parameters.  Ze is bedoeld om te gebruiken als bouwsteen in andere functies waar de gegevens voor bijvoorbeeld een welbepaalde versie of welbepaalde habitattypes geselecteerd moeten kunnen worden.  
#'
#' @template Zoekparameters
#'
#' @param ConnectieLSVIhabitats Connectie met de databank met indicatoren voor de LSVI van habitats, in te stellen d.m.v. functie connecteerMetLSVIdb.
#' @param Versie De versie van het LSVI-rapport, bv. "Versie 2" of "Versie 3".  Bij de default "alle" worden de gegevens voor de verschillende versies gegeven.
#' @param Habitatgroep Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen geselecteerd worden, bv. "Bossen", "Heiden", "(Half-)natuurlijke graslanden", "Zoete wateren",...   en "alle" (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type geselecteerd wordt.
#' @param Habitattype Parameter waarmee een habitattype kan geselecteerd worden.  Als dit habitattype meerdere subtypes heeft, zullen de gegevens van alle subtypes van dit habitattype weergegeven worden.
#' @param Habitatsubtype Parameter waarmee een habitatsubtype geselecteerd kan worden.  Als deze waarde ingevuld is, is het niet nodig om de parameters Habitatgroep en Habitattype te specifieren.
#' @param Criterium Het LSVI-criterium waarvoor de gegevens geselecteerd worden: "Vegetatie", "Structuur", "Verstoring" of "alle".
#' @param Indicator De indicator waarvoor de gegevens uit de databank gehaald worden.
#' @param HabitatnamenToevoegen Moeten de namen van de habitattypen en habitatsubtypen toegevoegd worden als extra kolommen?
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, Indicator_habitatID, SoortengroepID en NiveauSoortenlijstFiche.
#' 
#' @examples
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' selecteerIndicatoren(ConnectieLSVIhabitats, Versie = "Versie 3", Habitattype = "4010")
#' library(RODBC)
#' odbcClose(ConnectieLSVIhabitats)
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom assertthat assert_that
#'
#'
selecteerIndicatoren <- 
  function(ConnectieLSVIhabitats,
           Versie = "alle", 
           Habitatgroep = "alle",  
           Habitattype = "alle", 
           Habitatsubtype = "alle", 
           Criterium = "alle", 
           Indicator = "alle",
           HabitatnamenToevoegen = FALSE){
    
    assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))
    
    assert_that(is.string(Versie))
    if(!(Versie %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Versie","VersieLSVI"))){
      stop(sprintf("Versie moet een van de volgende waarden zijn: %s", 
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Versie","VersieLSVI")))
    }
    
    assert_that(is.string(Habitatgroep))
    if(!(Habitatgroep %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Habitatgroep","Habitatgroepnaam"))){
      stop(sprintf("Habitatgroep moet een van de volgende waarden zijn: %s", 
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Habitatgroep","Habitatgroepnaam")))
    }
    
    Habitattype <- ifelse(is.numeric(Habitattype),
                          as.character(Habitattype),
                          Habitattype)
    assert_that(is.string(Habitattype))
    if(!(Habitattype %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Habitattype","Habitatcode"))){
      stop(sprintf("Habitattype moet een van de volgende waarden zijn: %s", 
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Habitattype","Habitatcode")))
    }
    
    Habitatsubtype <- ifelse(is.numeric(Habitatsubtype),
                          as.character(Habitatsubtype),
                          Habitatsubtype)
    assert_that(is.string(Habitattype))
    if(!(Habitatsubtype %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Habitatsubtype","Habitatcode_subtype"))){
      stop(sprintf("Habitatsubtype moet een van de volgende waarden zijn: %s", 
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Habitatsubtype","Habitatcode_subtype")))
    }
    
    assert_that(is.string(Criterium))
    if(!(Criterium %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Criterium","Naam"))){
      stop(sprintf("Criterium moet een van de volgende waarden zijn: %s", 
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Criterium","Naam")))
    }
    
    assert_that(is.string(Indicator))
    if(!(Indicator %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Indicator","Naam"))){
      stop(sprintf("Indicator moet een van de volgende waarden zijn: %s", 
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Indicator","Naam")))
    }
    
    assert_that(is.logical(HabitatnamenToevoegen))
    
    
    query_uitbreiding <- ifelse(HabitatnamenToevoegen,
                                "Habitattype.Habitatnaam, 
    Habitatsubtype.Habitatnaam_subtype AS Habitatsubtypenaam,
    Habitatsubtype.Omschrijving AS HabitatsubtypeOmschrijving,
                                Habitatgroep.Habitatgroepnaam,",
                                "")
    
    #eerst de selectiegegevens ophalen en de nodige gegevens uit tabel Indicator_habitat, query samenstellen op basis van parameters
    Parametervoorwaarde <- FALSE
    query <- sprintf("SELECT Versie.VersieLSVI AS Versie, Habitattype.Habitatcode AS Habitattype, 
    Habitatsubtype.Habitatcode_subtype AS Habitatsubtype,
    %s
    Criterium.Naam AS Criterium, Indicator.Naam AS Indicator, 
    Indicator_habitat.Id AS Indicator_habitatID,
    Indicator_habitat.SoortengroepID, Indicator_habitat.NiveauSoortenlijstFiche,
    IndicatortabellenKoppeling.Indicator_beoordelingID
    FROM (((Indicator_habitat 
    INNER JOIN ((Habitatsubtype INNER JOIN Habitattype ON Habitatsubtype.HabitattypeID = Habitattype.Id)
    INNER JOIN Habitatgroep ON Habitattype.HabitatgroepID = Habitatgroep.Id)
    ON Indicator_habitat.HabitatsubtypeID = Habitatsubtype.Id)
    INNER JOIN (Indicator INNER JOIN Criterium ON Indicator.CriteriumID = Criterium.Id)
    ON Indicator_habitat.IndicatorID = Indicator.Id)
    INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.Id)
    LEFT JOIN IndicatortabellenKoppeling
    ON Indicator_habitat.Id = Indicator_habitatID",
                     query_uitbreiding)
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
    
    Selectiegegevens <- sqlQuery(ConnectieLSVIhabitats, query, stringsAsFactors = FALSE)
    
    return(Selectiegegevens)  
    
  }

