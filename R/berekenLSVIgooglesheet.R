#' @title Berekent de LSVI op basis van de opnames in googlesheet
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding voor alle gegevens in de opgegeven googlesheet-file.  Ze maakt hiervoor gebruik van de generieke berekenfunctie berekenLSVI().  Of dit was toch de bedoeling.  Doordat in de googlesheet spaties gebruikt zijn in de invoer en ID niet de eerste kolom, is het niet evident om deze rechtstreeks in te lezen in R.  We gebruiken daarom een 'opgepoetste versie' van een deel van de gegevens.  Die opgepoetste versie blijkt ook nog lastig om om te zetten, misschien beter wachten op de databank en voorlopig werken met al gelayoute gegevens.
#' 
#' Voor elk type gegevens (elke databank) moet een gelijkaardig script als dit geschreven worden om de gegevens naar de juiste vorm om te zetten.  Dit om te ondervangen dat voor sommige gegevens de bedekkingen per indicator ingeschat zijn, voor andere per soort.  Dit biedt ook de mogelijkheid om de bedekkingsschalen te vertalen enz.
#'
#' @inheritParams berekenLSVI
#' 
#' @param Data_indicatoren Gegevens over de indicatoren in de vorm van een data.frame met velden ID, Habitatsubtype, Criterium, Indicator en Waarde.
#' @param  Data_soorten Bedekkingen van de sleutelsoorten in de vorm van een data.frame met velden ID, Habitatsubtype, Soort_NL of Soort_Latijn en Bedekking.
#' 
#' @inheritParams connecteerMetLSVIdb
#'
#' @return Deze functie genereert de resultaten in de vorm van een tabel met de meetresultaten en scores per indicator.
#' 
#' @examples 
#' \dontrun{
#' Data_indicatoren <- read.csv2(system.file("vbdata/opname_4010_gelayout_indicatoren.csv", package = "LSVI"), stringsAsFactors = FALSE)
#' Data_soorten <- read.csv2(system.file("vbdata/opname_4010_gelayout_soorten.csv", package = "LSVI"), stringsAsFactors = FALSE)
#' berekenLSVIgooglesheet(Versie = "Versie 3", Kwaliteitsniveau = "1", Data_indicatoren, Data_soorten)
#' }
#'
#' @export
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr %>% filter_
#'
#'
berekenLSVIgooglesheet <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"),
           Kwaliteitsniveau = c("alle", "1", "2"),
           Data_indicatoren,
           Data_soorten,
           Server = "inbosql03\\prd",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "D0122_AppR",
           Wachtwoord = "***REMOVED***"){
    match.arg(Versie)
    match.arg(Kwaliteitsniveau)
    assert_that(inherits(Data_indicatoren, "data.frame"))
    assert_that(has_name(Data_indicatoren, "ID"))
    assert_that(has_name(Data_indicatoren, "Habitatsubtype"))
    assert_that(has_name(Data_indicatoren, "Criterium"))
    assert_that(has_name(Data_indicatoren, "Indicator"))
    assert_that(has_name(Data_indicatoren, "Waarde"))
    assert_that(inherits(Data_soorten, "data.frame"))
    assert_that(has_name(Data_soorten, "ID"))
    assert_that(has_name(Data_soorten, "Habitatsubtype"))
    assert_that(has_name(Data_soorten, "Soort_NL") | has_name(Data_soorten, "Soort_Latijn"))
    assert_that(has_name(Data_soorten, "Bedekking"))
    
    #eerst de criteria voor de sleutelsoorten testen op basis van Data_indicatoren (m.b.v. nog te maken functie)
    #dit resultaat toevoegen aan tabel Data_indicatoren
    
    #dan de LSVI berekenen voor Data_indicatoren met functie berekenLSVI()
    
  }
    