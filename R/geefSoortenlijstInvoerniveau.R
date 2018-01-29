#' @title Genereert soorten(groep)lijst(en) LSVI op basis van SoortengroepID en Niveau
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) uit de databank met de criteria en indicatoren voor de bepaling van de Lokale Staat van Instandhouding.  Het is in feite een hulpfunctie die voor verschillende andere functies gebruikt wordt en die de complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een opgegeven SoortengroepID (en in die zin iets minder gebruiksvriendelijk is).  Voor een selectie van soortenlijsten op basis van specifieke parameters is de functie geefSoortenlijst() een beter alternatief.
#' 
#' Deze functie geeft voor de gespecifieerde soortengroepen een lijst van soorten of soortengroepen op het gekozen niveau (dus soms tot op soortniveau, soms tot op genusniveau of hoger).  Om dezelfde soortenlijst als de habitatfiches te bekomen, moet het niveau vermeld worden dat in de tabel Indicator_habitat in de LSVI-databank vermeld is (en de SoortengroepID's die hierbij vermeld zijn).  Een lager nummer geeft een meer generieke groepering (dichter bij de opgegeven SoortengroepIDs), een hoger nummer een meer specifiek niveau (dichter bij het soortniveau).  Het maximale aantal niveaus of het niveau waarop het soortniveau gespecifieerd is, is indicatorspecifiek en afhankelijk van het aantal groepen of subgroepen dat in de LSVI gedefinieerd zijn (bv. opdeling van sleutelsoorten in bomen en kruiden of vermelding van genus i.p.v. soort zorgen elk voor een extra niveau).  Om een lijst van soorten op soortniveau te bekomen, wordt beter de functie geefSoortenlijstSoortniveau() gebruikt.   
#'
#'
#' @inheritParams selecteerIndicatoren
#' @param Soortengroeplijst dataframe waarin per niveau aangegeven wordt (tabel Niveau met een int die het niveau aangeeft) welke soortengroepen geselecteerd moeten worden (tabel SoortengroepIDs met een string waarin de ID's na mekaar weergegeven worden, gescheiden door een komma)
#'
#' @return Deze functie geeft een tabel met velden SoortengroepID, evt. SoortensubgroepID, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).  SoortensubgroepID wordt enkel gegeven als het record een minder diep niveau betreft dan het soortniveau en is het SoortengroepID van het niveau van het record, dus van een niveau dieper dan SoortengroepID.
#' 
#' @examples
#' Soortengroeplijst <- 
#'      data.frame(Niveau = c(1, 2), 
#'                 SoortengroepIDs = c("139,142,372", "370"), 
#'                 stringsAsFactors = FALSE)
#' geefSoortenlijstInvoerniveau(Soortengroeplijst)
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows mutate_
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom assertthat assert_that noNA is.count has_name 
#'
#'
geefSoortenlijstInvoerniveau <- 
  function(Soortengroeplijst,
           ConnectieLSVIhabitats = connecteerMetLSVIdb()){
    
    assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))
    assert_that(inherits(Soortengroeplijst, "data.frame"))
    assert_that(has_name(Soortengroeplijst, "Niveau"))
    assert_that(has_name(Soortengroeplijst, "SoortengroepIDs"))
    assert_that(is.character(Soortengroeplijst$SoortengroepIDs))
    assert_that(noNA(Soortengroeplijst$SoortengroepIDs))
    if (!all(grepl("^([[:digit:]]+,)*[[:digit:]]+$", Soortengroeplijst$SoortengroepIDs))) {
      stop("Niet alle SoortengroepIDs bestaan uit een reeks getallen gescheiden door een komma")
    }
    
    for (i in seq(nrow(Soortengroeplijst))) {
      Soortengroeplijst$Niveau[i] <- 
        ifelse(is.string(Soortengroeplijst$Niveau[i]),
               as.numeric(Soortengroeplijst$Niveau[i]),
               Soortengroeplijst$Niveau[i])
      assert_that(is.count(Soortengroeplijst$Niveau[i]))
    }
    
    #voor elk niveau de gegevens ophalen op basis van een query samengesteld op basis van het niveau, en deze gegevens aan elkaar plakken
    #met 1 query lukt het niet om Omschrijving op de verschillende niveaus binnen te halen, dus we beperken ons tot een omschrijving op het niveau net boven het niveau van de vermelde soort(engroep)
    Soortenlijst <- NULL
    for (n in Soortengroeplijst$Niveau) {
      query <- 
        sprintf("WITH Soortengroepniveau
                AS
                (
                  SELECT Soortengroep.Id as SoortengroepID, Soortengroep.Omschrijving, 
                       SoortengroepSoort.SoortensubgroepID, SoortengroepSoort.SoortID, 0 AS Niveau
                  FROM Soortengroep INNER JOIN SoortengroepSoort ON Soortengroep.Id = SoortengroepSoort.SoortengroepID
                  WHERE Soortengroep.Id in (%s)
                  UNION ALL
                  SELECT Soortengroepniveau.SoortengroepID, s2.Omschrijving,
                       ss2.SoortensubgroepID, ss2.SoortID, Niveau + 1
                  FROM (Soortengroep AS s2 INNER JOIN SoortengroepSoort AS ss2 ON s2.Id = ss2.SoortengroepID)
                  INNER JOIN Soortengroepniveau ON s2.Id = Soortengroepniveau.SoortensubgroepID
                  WHERE Niveau < %s
                )
                SELECT Soortengroepniveau.SoortengroepID, Soortengroepniveau.SoortensubgroepID,  
                       Soortengroepniveau.Omschrijving,
                       Soortengroep.WetNaam AS WetNaam_groep, Soortengroep.Naam AS NedNaam_groep, 
                       Soort.WetNaam, Soort.NedNaam, Soortengroepniveau.Niveau
                FROM Soortengroepniveau 
                LEFT JOIN Soortengroep ON Soortengroepniveau.SoortensubgroepID = Soortengroep.Id
                LEFT JOIN Soort ON Soortengroepniveau.SoortID = Soort.Id
                WHERE Soortengroepniveau.Niveau = %s", 
                Soortengroeplijst[Soortengroeplijst$Niveau == n,"SoortengroepIDs"], n, n - 1)

      Soortenlijst_n <- sqlQuery(ConnectieLSVIhabitats, query, stringsAsFactors = FALSE)
      
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
        NedNaam_groep = ~NULL,
        Niveau = ~NULL
      ) 
    
    #kolommen wissen die enkel NA's bevatten: geeft problemen voor andere functies: toch maar niet doen
    #Soortenlijst <- Filter(function(x)!all(is.na(x)),Soortenlijst)
    
    
    return(Soortenlijst)  
  }

