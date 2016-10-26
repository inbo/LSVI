#' @title Genereert soortenlijst(en) LSVI op basis van SoortengroepID
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) uit de databank met de criteria en indicatoren voor de bepaling van de Lokale Staat van Instandhouding.  Het is in feite een hulpfunctie die voor verschillende andere functies gebruikt wordt en die de complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een opgegeven SoortengroepID (en in die zin iets minder gebruiksvriendelijk is).  Voor een selectie van soortenlijsten op basis van specifieke parameters is de functie geefSoortenlijst() een beter alternatief.
#' 
#' Deze functie geeft voor de gespecifieerde soortengroepen per soortengroep een lijst van alle soorten die hieronder vallen.  Dit betekent dat voor de in de LSVI vermelde genera en soortengroepen als helofieten alle soorten vermeld worden die tot deze groepen behoren.  Voor een lijst van soort(groep)en op het niveau vermeld in de LSVI, wordt beter de functie geefSoortenlijstInvoerniveau() gebruikt.
#'
#' @param Soortengroeplijst string waarin de SoortengroepID's na elkaar weergegeven worden, gescheiden door een komma
#'
#' @return Deze functie geeft een tabel met velden SoortengroepID, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#' 
#' @examples
#' geefSoortenlijstSoortniveau("139,142,372,370")
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows mutate_
#' @importFrom RODBC sqlQuery odbcClose
#'
#'
geefSoortenlijstSoortniveau <- 
  function(Soortengroeplijst){
    #nog controle doen op de invoer!!!
    
    query <- 
      sprintf("WITH Soortengroepniveau
              AS
              (
                SELECT Soortengroep.Id as SoortengroepID, 
                     SoortengroepSoort.SoortensubgroepID, SoortengroepSoort.SoortID, 0 AS Niveau
                FROM Soortengroep INNER JOIN SoortengroepSoort ON Soortengroep.Id = SoortengroepSoort.SoortengroepID
                WHERE Soortengroep.Id in (%s)
                UNION ALL
                SELECT Soortengroepniveau.SoortengroepID,
                     ss2.SoortensubgroepID, ss2.SoortID, Niveau + 1
                FROM (Soortengroep AS s2 INNER JOIN SoortengroepSoort AS ss2 ON s2.Id = ss2.SoortengroepID)
                INNER JOIN Soortengroepniveau ON s2.Id = Soortengroepniveau.SoortensubgroepID
              )
              SELECT Soortengroepniveau.SoortengroepID, 
                     Soort.WetNaam, Soort.NedNaam
              FROM Soortengroepniveau 
              INNER JOIN Soort ON Soortengroepniveau.SoortID = Soort.Id", 
              Soortengroeplijst)
    
    
    connectie <- connecteerMetLSVIdb()
    Soortenlijst <- sqlQuery(connectie, query, stringsAsFactors = FALSE)
    odbcClose(connectie)

     
    #Kolommen met WetNaam (uit tabellen Soortengroep en Soort) samenvoegen, id voor NedNaam, en een kolom WetNaamKort toevoegen
    Soortenlijst <- Soortenlijst %>%
      mutate_(
        WetNaamKort = ~
          gsub(
            pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
            replacement = "\\1 \\2",
            x = WetNaam
          )
      )

    return(Soortenlijst)
  }

