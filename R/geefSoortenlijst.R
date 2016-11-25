#' @title Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de opgegeven parameters.  In feite genereert ze een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam waarin de gespecificeerde parameters uitgeselecteerd zijn en waar voor andere parameters alle waarden uit de databank weergegeven zijn.  
#' 
#' Er zijn 2 opties: de soorten weergeven zoals in de habitatfiches (op soortniveau, genusniveau of hoger niveau, zoals het in de habitatfiches vermeld is) of alle soorten op soortniveau weergeven en dus bij soortengroepen alle mogelijke soorten van deze groep weergeven.  Deze opties kunnen opgegeven worden in de parameter Soortenlijsttype.
#'
#'De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
#'
#' @inheritParams selecteerIndicatoren
#' @param Soortenlijsttype "LSVIfiche" betekent dat de soortenlijst van de habitatfiche wordt overgenomen, "Soortniveau" betekent dat alle soorten worden weergegeven die in de groepen vallen die aan de parameters voldoen (bv. alle soorten bomen en struiken als dit in LSVI-fiche vermeld is), "alle" betekent dat alle soorten en alle taxonomische en morfologische groepen worden weergegeven die volledig in de groepen vallen die aan de parameters voldoen (dus gelijkaardig als Soortniveau, maar dan uitgebreid naar hogere taxonomische en morfologische groepen).
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#' 
#' @examples
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "LSVIfiche")
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "Soortniveau")
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "alle")
#'
#' @export
#'
#' @importFrom dplyr %>% select_ distinct_ filter group_by_ summarise_ ungroup mutate_ left_join rename_
#' @importFrom RODBC sqlQuery odbcClose
#'
#'
geefSoortenlijst <- 
  function(Versie = geefUniekeWaarden("Versie","VersieLSVI"), 
           Habitatgroep = geefUniekeWaarden("Habitatgroep","Habitatgroepnaam"),  
           Habitattype = geefUniekeWaarden("Habitattype","Habitatcode"), 
           Habitatsubtype = geefUniekeWaarden("Habitatsubtype","Habitatcode_subtype"), 
           Criterium = geefUniekeWaarden("Criterium","Naam"), 
           Indicator = geefUniekeWaarden("Indicator","Naam"),
           Soortenlijsttype = c("LSVIfiche", "Soortniveau", "alle")){
    
    match.arg(Soortenlijsttype)
    
    Selectiegegevens <- selecteerIndicatoren(Versie, Habitatgroep, Habitattype, Habitatsubtype, Criterium, Indicator)
    
    #nu de soortgegevens ophalen:
    if(Soortenlijsttype[1] == "LSVIfiche"){
      #eerst oplijsten welke gegevens moeten opgehaald worden per niveau van Soortengroep en SoortengroepSoort
      SoortengroepIDperNiveau <- Selectiegegevens %>%
        select_(~SoortengroepID, ~NiveauSoortenlijstFiche) %>%
        distinct_() %>%
        filter(!is.na(SoortengroepID)) %>%
        group_by_(~NiveauSoortenlijstFiche) %>%
        summarise_(SoortengroepIDs = ~ paste(SoortengroepID, collapse=",")) %>%
        ungroup() %>%
        rename_(Niveau = ~NiveauSoortenlijstFiche)
      
      #dan voor elk niveau de gegevens ophalen
      Soortenlijst <- geefSoortenlijstInvoerniveau(SoortengroepIDperNiveau)
      
    } else if(Soortenlijsttype == "Soortniveau" | Soortenlijsttype == "alle"){
      #de andere optie: gegevens van het diepste niveau ophalen
      SoortengroepIDs <- Selectiegegevens %>%
        select_(~SoortengroepID) %>%
        distinct_() %>%
        filter(!is.na(SoortengroepID)) %>%
        summarise_(SoortengroepIDs = ~ paste(SoortengroepID, collapse=","))
      
      Soortenlijst <- 
        geefSoortenlijstSoortniveau(Soortengroeplijst = SoortengroepIDs$SoortengroepIDs,
                                    Soortenlijsttype = Soortenlijsttype)
      
    } 
    
    
    #soortgegevens aan selectiegegevens plakken
    SoortenlijstSelectie <- Selectiegegevens %>%
      left_join(Soortenlijst, by = ("SoortengroepID" = "SoortengroepID")) %>%
      mutate_(
        NiveauSoortenlijstFiche = ~NULL
      )
    
    return(SoortenlijstSelectie)  
    
  }

