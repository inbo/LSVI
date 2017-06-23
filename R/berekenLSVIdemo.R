#' @title Berekent de LSVI op basis van de demo-opnames
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding voor de testdata in dit package.  Bedoeling van deze functie is om met een eenvoudig voorbeeld te tonen hoe aan de hand van functies uit het LSVI-package een 'rekenmodule' gemaakt kan worden voor het berekenen van de LSVI van een opname.
#'
#' Het zou mooi zijn om op termijn via dit package gelijkaardige rekenmodules aan te kunnen bieden voor de berekening van de LSVI voor alle relevante databanken die op het inbo gebruikt worden.  Daarom een oproep om bij het ontwikkelen van scripts, deze te bezorgen aan de beheerder van het package, zodat ze beschikbaar gesteld kunnen worden via het package.
#'
#' Dus voor elk type opname-gegevens (elke databank) moet een gelijkaardig script als dit geschreven worden om de gegevens naar de juiste vorm om te zetten.  Dit om te ondervangen dat voor sommige gegevens de bedekkingen per indicator ingeschat zijn, voor andere per soort.  Dit biedt ook de mogelijkheid om de bedekkingsschalen te vertalen enz.
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams berekenLSVIbasis
#'
#' @param Data_indicatoren Gegevens over de indicatoren in de vorm van een data.frame met velden ID, Habitattype, Criterium, Indicator en Waarde, waarbij ID een groeperende variabele is voor een opname (plaats en tijdstip) en Waarde de waarde die voor die indicator geobserveerd of gemeten is.  Habitatsutbype, Criterium en Indicator moeten overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door resp. geefUniekeWaarden("Habitatsubtype", "Habitatcode_subtype"), geefUniekeWaarden("Criterium", "Naam") en geefUniekeWaarden("Indicator", "Naam")).  Waarde moet voldoen aan de beschrijving die opgevraagd kan worden met geefInvoervereisten().
#'
#' @param  Data_soorten Bedekkingen van de sleutelsoorten in de vorm van een data.frame met velden ID, Habitattype, Soort_NL of Soort_Latijn en Bedekking.
#'
#' @return Deze functie genereert de resultaten in de vorm van een list met 3 tabellen: een eerste met de beoordelingen per criterium en kwaliteitsniveau, een tweede met de beoordelingen per indicator en kwaliteitsniveau, en een derde met de detailgegevens inclusief meetwaarden.
#'
#' @examples
#' library(readr)
#' Data_indicatoren <-
#'     read_csv2(system.file("vbdata/opname_4010_gelayout_indicatoren.csv", package = "LSVI"))
#' Data_soorten <-
#'     read_csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"))
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' berekenLSVIdemo(ConnectieLSVIhabitats, Versie = "Versie 3", Kwaliteitsniveau = "1",
#'                 Data_indicatoren, Data_soorten)
#' library(RODBC)
#' odbcClose(ConnectieLSVIhabitats)
#' plot(1)
#'
#' @export
#'
#' @importFrom readr read_csv2
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate_ left_join group_by_ summarise_ ungroup
#' @importFrom RODBC odbcClose
#'
#'
berekenLSVIdemo <-
  function(ConnectieLSVIhabitats,
           Versie = "alle",
           Kwaliteitsniveau = "alle",
           Data_indicatoren,
           Data_soorten){

    #omdat we voor de opnames een bedekkingsschaal hebben gebruikt die afwijkt van de gebruikte schalen in het package (% en Tansley), halen we eerst even de tabel op waarin we een vertaling gemaakt hebben tussen de gebruikte schaal en hetgeen voor de functies nodig is (% en Tansley)
    Schaalomzetting <-
      read_csv2(system.file("schaaltabellen/Schaalomzetting_ToonS.csv", package = "LSVI"))


    #we testen even of de ingevoerde gegevens wel het juiste formaat hebben, om te vermijden dat de functie zich onvoorspelbaar gedraagt (foute uitvoer, vastlopen, cryptische foutmelding,...) als een gebruiker van de functie een verkeerde parameter invoert (gebruiksvriendelijkheid)
    assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))

    assert_that(is.string(Versie))
    if(!(Versie %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Versie","VersieLSVI"))){
      stop(sprintf("Versie moet een van de volgende waarden zijn: %s",
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Versie","VersieLSVI")))
    }

    Kwaliteitsniveau <- ifelse(Kwaliteitsniveau==1, "1",
                               ifelse(Kwaliteitsniveau==2, "2",
                                      Kwaliteitsniveau))
    assert_that(is.string(Kwaliteitsniveau))
    if(!(Kwaliteitsniveau %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Beoordeling",
                                                 "Kwaliteitsniveau"))){
      stop(sprintf("Kwaliteitsniveau moet een van de volgende waarden zijn: %s",
                   geefUniekeWaarden(ConnectieLSVIhabitats,"Beoordeling","Kwaliteitsniveau")))
    }

    assert_that(inherits(Data_indicatoren, "data.frame"))
    assert_that(has_name(Data_indicatoren, "ID"))
    assert_that(has_name(Data_indicatoren, "Habitattype"))
    if (!all(Data_indicatoren$Habitattype %in%
            geefUniekeWaarden(ConnectieLSVIhabitats, "Habitattype", "Code"))) {
      stop("Niet alle waarden vermeld onder Data_indicatoren$Habitattype zijn habitat(sub)types.")
    }
    assert_that(has_name(Data_indicatoren, "Criterium"))
    if (!all((tolower(Data_indicatoren$Criterium)) %in% tolower(geefUniekeWaarden(ConnectieLSVIhabitats, "Criterium", "Naam")))) {
      stop("Niet alle waarden vermeld onder Data_indicatoren$Criterium komen overeen met waarden vermeld in de databank.")
    }
    assert_that(has_name(Data_indicatoren, "Indicator"))
    if (!all((tolower(Data_indicatoren$Indicator)) %in% tolower(geefUniekeWaarden(ConnectieLSVIhabitats, "Indicator", "Naam")))) {
      stop("Niet alle waarden vermeld onder Data_indicatoren$Indicator komen overeen met waarden vermeld in de databank.")
    }
    assert_that(has_name(Data_indicatoren, "Waarde"))
    #de functie berekenLSVIbasis, waaraan 'Waarde' zonder voorafgaande bewerkingen doorgegeven wordt, test het datatype van Waarde, dus wij moeten dit niet doen

    assert_that(inherits(Data_soorten, "data.frame"))
    assert_that(has_name(Data_soorten, "ID"))
    assert_that(has_name(Data_soorten, "Habitattype"))
    if (!all(Data_soorten$Habitattype %in%
             geefUniekeWaarden(ConnectieLSVIhabitats, "Habitattype", "Code"))) {
      stop("Niet alle waarden vermeld onder Data_soorten$Habitattype zijn habitat(sub)types.")
    }
    assert_that(has_name(Data_soorten, "Soort_NL") | has_name(Data_soorten, "Soort_Latijn"))
    #we geven deze soortenlijsten rechtstreeks door aan de functie berekenAnalyseVariabele, die de juist invoer van deze lijsten grondig test (dus we moeten het hier niet doen)
    assert_that(has_name(Data_soorten, "Bedekking"))
    if (!all(Data_soorten$Bedekking %in% Schaalomzetting$Schaal_opname)) {
      stop("Niet alle waarden vermeld onder Data_soorten$Bedekking komen overeen met de bedekkingsschaal die gebruikt wordt voor deze monitoring.")
    }


    #Bon, nu op naar het echte werk!
    #In de helpfunctie van de functie berekenLSVIbasis() lezen we dat we onze gegevens in een welbepaald formaat moeten zijn en dat we ze moeten koppelen aan VoorwaardeID aan de hand van de info die met geefInvoervereisten() gegeven wordt.  Eens kijken wat we nodig hebben voor onze dataset:

    Invoervereisten <-
      geefInvoervereisten(ConnectieLSVIhabitats,
                          Versie = Versie,
                          Habitattype = unique(c(Data_indicatoren$Habitattype,
                                                 Data_soorten$Habitattype)),
                          Kwaliteitsniveau = Kwaliteitsniveau)


    #In dit demo-voorbeeld is de koppeling vrij eenvoudig te maken omdat de opnamen gemaakt zijn met een LSVI-bepaling als doel en de gegevens voor een groot deel in het juiste formaat zijn, maar in veel gevallen zullen een aantal variabelen 'handmatig' gekoppeld moeten worden en mogelijk moeten eerst berekeningen uitgevoerd worden.  Dit is in elk geval de stap die het meeste werk en creativiteit vraagt.  Enkele tips:
    # - check of je gegevens omgezet kunnen worden naar AnalyseVariabelen, deze beperkte set aan variabelen zijn niet habitatspecifiek, wat een generieke aanpak mogelijk maakt
    # - maak waar zinvol een 'vertaaltabel' van variabelenamen of categorienamen zoals hier gedaan is voor de bedekkingsschalen
    # - maak gebruik van de functie berekenAnalyseVariabele voor gegevens bestaande uit soortenlijsten of categorische variabelen


    #De tactiek die we hier gebruiken:
    # - eerst Data_indicatoren koppelen aan de Invoervereisten (eerst hoofdletterprobleem van Criterium oplossen)
    # - de eerder binnengehaalde tabel Schaalomzetting toevoegen
    # - kiezen tussen % en Tansley op basis van TypeVariabele
    # - de nog niet ingevulde waarden berekenen met berekenAnalyseVariabele op basis van de soortenlijst

    Invoerdata <- Invoervereisten %>%
      mutate_(
        Criterium = ~tolower(Criterium)
      ) %>%
      left_join(Data_indicatoren %>%
                  mutate_(
                    Criterium = ~tolower(Criterium),
                    Indicator = ~tolower(Indicator)
                  ),
                by = c("Habitattype" = "Habitattype",
                       "Criterium" = "Criterium",
                       "Indicator" = "Indicator")) %>%
      left_join(Schaalomzetting,
                by = c("Waarde" = "Schaal_opname")) %>%
      mutate_(
        Waarde = ~ifelse(TypeVariabele == "Categorie",
                         Tansley,
                         ifelse(TypeVariabele == "Percentage",
                                Percentage,
                                Waarde))
      )

    #bij de ontbrekende gegevens zou nog een opsplitsing gemaakt moeten worden tussen soortenlijsten en andere lijsten, zeker die met SoortengroepID eruit halen!
    Ontbrekend <- Invoerdata %>%
      filter_(~is.na(Waarde)) %>%
      filter_(~!is.na(AnalyseVariabele)) %>%
      group_by_(~AnalyseVariabele) %>%
      summarise_(
        Soortengroeplijst = ~paste(unique(SoortengroepID), collapse = ",")
      ) %>%
      ungroup


    Data_soorten <- Data_soorten %>%
      left_join(Schaalomzetting, by = c("Bedekking" = "Schaal_opname"))

    for(i in seq_len(nrow(Ontbrekend))){
      Data <- berekenAnalyseVariabele(ConnectieLSVIhabitats,
                                      Ontbrekend$AnalyseVariabele[i],
                                      Data_soorten,
                                      Ontbrekend$Soortengroeplijst[i]) %>%
        mutate_(
          AnalyseVariabele = ~Ontbrekend$AnalyseVariabele[i],
          IDdata = ~ID,
          ID = ~NULL,
          WaardeData = ~Waarde,
          Waarde = ~NULL
        )
      Invoerdata <- Invoerdata %>%
        left_join(Data, by = c("AnalyseVariabele" = "AnalyseVariabele",
                               "SoortengroepID" = "SoortengroepID")) %>%
        mutate_(
          ID = ~ifelse(is.na(ID), IDdata, ID),
          Waarde = ~ifelse(is.na(Waarde), WaardeData, Waarde),
          IDdata = ~NULL,
          WaardeData = ~NULL
        )
    }

    Resultaat <-
      berekenLSVIbasis(ConnectieLSVIhabitats, Versie, Kwaliteitsniveau,
                       Invoerdata %>%
                         select_(~ID, ~Habitattype, ~VoorwaardeID, ~Waarde) %>%
                         filter_(~!is.na(ID)))

    return(Resultaat)

  }
