#' @title Genereert soorten(groep)lijst(en) LSVI op basis van SoortengroepID en Niveau
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) uit de databank met de criteria en indicatoren voor de bepaling van de Lokale Staat van Instandhouding.  Het is in feite een hulpfunctie die voor verschillende andere functies gebruikt wordt en die de complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een opgegeven SoortengroepID (en in die zin iets minder gebruiksvriendelijk is).  Voor een selectie van soortenlijsten op basis van specifieke parameters is de functie geefSoortenlijst() een beter alternatief.
#' 
#' Deze functie geeft voor de gespecifieerde soortengroepen een lijst van soorten of soortengroepen op het gekozen niveau (dus soms tot op soortniveau, soms tot op genusniveau of hoger).  Om dezelfde soortenlijst als de habitatfiches te bekomen, moet het niveau vermeld worden dat in de tabel Indicator_habitat in de LSVI-databank vermeld is (en de SoortengroepID's die hierbij vermeld zijn).  Een lager nummer geeft een meer generieke groepering, een hoger nummer een meer specifiek niveau.  Het maximale aantal niveaus of het niveau waarop het soortniveau gespecifieerd is, is indicatorspecifiek en afhankelijk van het aantal groepen of subgroepen dat in de LSVI gedefinieerd zijn (bv. opdeling van sleutelsoorten in bomen en kruiden of vermelding van genus i.p.v. soort zorgen elk voor een extra niveau).  Om een lijst van soorten op soortniveau te bekomen, wordt beter de functie geefSoortenlijstSoortniveau() gebruikt.   
#'
#'
#' @param Soortengroeplijst dataframe waarin per niveau aangegeven wordt (tabel Niveau met een int die het niveau aangeeft) welke soortengroepen geselecteerd moeten worden (tabel SoortengroepIDs met een string waarin de ID's na mekaar weergegeven worden, gescheiden door een komma)
#'
#' @return Deze functie geeft een tabel met velden SoortengroepID, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#' 
#' @examples
#' Soortengroeplijst <- data.frame(Niveau = c(1, 2), SoortengroepIDs = c("139,142,372", "370"))
#' geefSoortenlijstInvoerniveau(Soortengroeplijst)
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows mutate_
#' @importFrom RODBC sqlQuery odbcClose
#'
#'
geefSoortenlijstInvoerniveau <- 
  function(Soortengroeplijst){
    #nog controle doen op de invoer!!!
    
    
    #voor elk niveau de gegevens ophalen op basis van een query samengesteld op basis van het niveau, en deze gegevens aan elkaar plakken
    Soortenlijst <- NULL
    for(n in Soortengroeplijst$Niveau){
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
                                       Soortengroeplijst[Soortengroeplijst$Niveau==n,"SoortengroepIDs"])

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
    
    
    return(Soortenlijst)  
  }

