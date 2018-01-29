#' @title Controle van de ingevoerde opname
#'
#' @description Deze hulpfunctie controleert of de ingevoerde opname geen fouten bevat en koppelt ze meteen aan de opgegeven soortenlijst.  Ze controleert of veld ID en een veld met een soortenlijst (soort_Latijn of soort_NL) aanwezig is, en controleert voor de soortenlijst of de namen wel bestaan (conform zijn met de LSVI-databank).  Problemen hiermee geven een error.  Verder controleert ze of alle soorten uit de opgegeven soortengroep(en) in de opname aanwezig zijn en ze geeft een warning als dit niet zo is.
#'
#' 
#' @inheritParams selecteerIndicatoren
#' @param  Data_soorten Bedekkingen van de sleutelsoorten in de vorm van een data.frame met velden ID en Soort_NL of Soort_Latijn.  Eventueel andere velden worden gewoon teruggegeven bij de uitvoer. (Eventueel zou hier ook de NBNTaxonVersionKey kunnen gebruikt worden.)
#' @inheritParams geefSoortenlijstSoortniveau
#' 
#' 
#' @return Deze functie geeft een aangepaste tabel Data_soorten terug waarin enkel de soorten uit de soortenlijst(en) opgenomen zijn en die bovendien gekoppeld is aan de gegevens van de soortenlijst.
#' 
#'
#' @export   
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% inner_join anti_join summarise_ select_
#' @importFrom RODBC sqlQuery
#'
#'
selecteerSoortenInOpname <- 
  function(Data_soorten, 
           Soortengroeplijst,
           ConnectieLSVIhabitats){
    
    assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))
    assert_that(inherits(Data_soorten, "data.frame"))
    assert_that(has_name(Data_soorten, "ID"))
    assert_that(has_name(Data_soorten, "Soort_NL") | has_name(Data_soorten, "Soort_Latijn"))
    
    #een soortenlijst met alle wetenschappelijke/Nederlandse namen uit de databank (zonder auteursnaam) om de schrijfwijze van de namen in de lijst te controleren
    query <- "SELECT Soortengroep.Naam, Soortengroep.WetNaam
          FROM Soortengroep INNER JOIN Soortengroeptype 
          ON Soortengroep.SoortengroeptypeID = Soortengroeptype.Id
          WHERE Soortengroeptype.Omschrijving <> 'Conceptueel'"

    Soortengroepenlijst <- sqlQuery(ConnectieLSVIhabitats, query, stringsAsFactors = FALSE)

    Soortenlijst_Latijn <- 
      c(
        gsub(
          pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
          replacement = "\\1 \\2",
          x = geefUniekeWaarden("Soort", "WetNaam", ConnectieLSVIhabitats)
        ),
        Soortengroepenlijst$WetNaam
      )
    Soortenlijst_NL <-
      c(geefUniekeWaarden("Soort", "NedNaam", ConnectieLSVIhabitats),
        Soortengroepenlijst$WetNaam)
      
    
    #de soorten van het eerste niveau uit de soortengroep ophalen
    Soortengroep <- 
      geefSoortenlijstInvoerniveau(
        data.frame(
          Niveau = 1,
          SoortengroepIDs = Soortengroeplijst,
          stringsAsFactors = FALSE
        ),
        ConnectieLSVIhabitats
      ) %>%
      select_(
        ~ SoortengroepID,
        ~ SoortensubgroepID,
        ~ WetNaamKort,
        ~ NedNaam
      )
    
    #als het Latijnse namen zijn: eerst auteursnamen verwijderen om vergelijking te vergemakkelijken
    if (has_name(Data_soorten, "Soort_Latijn")) {
      Data_soorten$WetNaamKort <- gsub(
        pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
        replacement = "\\1 \\2",
        x = Data_soorten$Soort_Latijn
      )
      #controleren of alle namen in de databank voorkomen, anders foutmelding geven
      if (!all(!is.na(Data_soorten$WetNaamKort) & Data_soorten$WetNaamKort %in% Soortenlijst_Latijn)) {
        stop("Niet alle waarden vermeld onder Data_soorten$Soort_Latijn komen overeen met wetenschappelijke namen van soorten in de databank.")
      } else {
        #testen of alle namen van de soortengroep in de opname voorkomen (indien niet, ook subniveaus testen), anders een waarschuwing geven
        if (!all(Soortengroep$WetNaamKort %in% Data_soorten$WetNaamKort)) {
          OntbrekendeSoorten <- Soortengroep %>%
            anti_join(Data_soorten, by = c("WetNaamKort" = "WetNaamKort"))
          if (has_name(OntbrekendeSoorten, "SoortensubgroepID") & 
             !all(is.na(OntbrekendeSoorten$SoortensubgroepID))) {
            Subsoorten <- OntbrekendeSoorten %>%
              filter_(~!is.na(SoortensubgroepID)) %>%
              summarise_(SoortensubgroepIDs = ~ paste(SoortensubgroepID, collapse=","))
            Subsoortengroep <- 
              geefSoortenlijstSoortniveau(
                Subsoorten$SoortensubgroepIDs,
                ConnectieLSVIhabitats
              ) %>%
              mutate_(
                WetNaam = ~ NULL,
                SoortensubgroepID = ~ SoortengroepID,
                SoortengroepID = ~ NULL
              ) %>%
              inner_join(
                OntbrekendeSoorten %>% 
                  select_(~ SoortengroepID, ~ SoortensubgroepID),
                by = c("SoortensubgroepID" = "SoortensubgroepID")
              ) %>%
              inner_join(
                Data_soorten %>%
                  select_(~ WetNaamKort) %>%
                  distinct_(), 
                by = c("WetNaamKort" = "WetNaamKort")
              )
            Soortengroep <- Soortengroep %>%
              bind_rows(Subsoortengroep)
            if (!all(OntbrekendeSoorten$SoortensubgroepID %in% Subsoortengroep$SoortensubgroepID)) {
              warning("Niet alle te evalueren soorten zijn opgenomen onder Data_soorten$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
            }
          } else {
            warning("Niet alle te evalueren soorten zijn opgenomen onder Data_soorten$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
          }
        }
        Koppeling <- c("WetNaamKort" = "WetNaamKort")
      } 
    } else {
      #en nu dezelfde procedure voor Nederlandse namen
      if (!all(!is.na(Data_soorten$Soort_NL) & Data_soorten$Soort_NL %in% Soortenlijst_NL)) {
        stop("Niet alle waarden vermeld onder Data_soorten$Soort_NL komen overeen met Nederlandse namen van soorten in de databank.")
      } else {
        if (!all(Soortengroep$NedNaam %in% Data_soorten$Soort_NL)) {
          OntbrekendeSoorten <- Soortengroep %>%
            anti_join(Data_soorten, by = c("NedNaam" = "Soort_NL"))
          if (has_name(OntbrekendeSoorten, "SoortensubgroepID") & 
             !all(is.na(OntbrekendeSoorten$SoortensubgroepID))) {
            Subsoorten <- OntbrekendeSoorten %>%
              filter_(~!is.na(SoortensubgroepID)) %>%
              summarise_(SoortensubgroepIDs = ~ paste(SoortensubgroepID, collapse=","))
            Subsoortengroep <- 
              geefSoortenlijstSoortniveau(
                Subsoorten$SoortensubgroepIDs,
                ConnectieLSVIhabitats
              ) %>%
              mutate_(
                WetNaam = ~ NULL,
                SoortensubgroepID = ~ SoortengroepID,
                SoortengroepID = ~ NULL
              ) %>%
              inner_join(
                OntbrekendeSoorten %>% 
                  select_(~ SoortengroepID, ~ SoortensubgroepID),
                by = c("SoortensubgroepID" = "SoortensubgroepID")
              ) %>%
              inner_join(
                Data_soorten %>%
                  select_(~ Soort_NL) %>%
                  distinct_(), 
                by = c("NedNaam" = "Soort_NL")
              )
            Soortengroep <- Soortengroep %>%
              bind_rows(Subsoortengroep)
            if (!all(OntbrekendeSoorten$SoortensubgroepID %in% Subsoortengroep$SoortensubgroepID)) {
              warning("Niet alle te evalueren soorten zijn opgenomen onder Data_soorten$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
            }
          } else {
            warning("Niet alle te evalueren soorten zijn opgenomen onder Data_soorten$Soort_NL, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn")
          }
        }
        Koppeling <- c("Soort_NL" = "NedNaam")
      }
    }
    
    Data_soorten <- Data_soorten %>%
      inner_join(Soortengroep, by = Koppeling) 
    
    return(Data_soorten)
  } 
