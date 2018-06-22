#' @title Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van de opgegeven parameters.  In feite genereert ze een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam waarin de gespecificeerde parameters uitgeselecteerd zijn en waar voor andere parameters alle waarden uit de databank weergegeven zijn.
#'
#' Er zijn 2 opties: de soorten weergeven zoals in de habitatfiches (op soortniveau, genusniveau of hoger niveau, zoals het in de habitatfiches vermeld is) of alle soorten op soortniveau weergeven en dus bij soortengroepen alle mogelijke soorten van deze groep weergeven.  Deze opties kunnen opgegeven worden in de parameter Soortenlijsttype.
#'
#' @template Zoekparameters
#'
#' @inheritParams selecteerIndicatoren
#' @param Soortenlijsttype "LSVIfiche" betekent dat de soortenlijst van de habitatfiche wordt overgenomen, "alle" betekent dat alle soorten en alle taxonomische groepen worden weergegeven die volledig in de groepen vallen die aan de parameters voldoen.
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#'
#' @examples
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "LSVIfiche")
#' geefSoortenlijst(Habitattype = "4010", Soortenlijsttype = "alle")
#'
#' @export
#'
#' @importFrom dplyr %>% select distinct filter group_by summarise ungroup mutate_ left_join rename
#' @importFrom rlang .data
#'
#'
geefSoortenlijst <-
  function(Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           Criterium = "alle",
           Indicator = "alle",
           Soortenlijsttype = c("LSVIfiche", "alle"),
           ConnectieLSVIhabitats = ConnectiePool){

    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )
    match.arg(Soortenlijsttype)

    Selectiegegevens <-
      selecteerIndicatoren(
        Versie = Versie,
        Habitatgroep = Habitatgroep,
        Habitattype = Habitattype,
        Criterium = Criterium,
        Indicator = Indicator,
        ConnectieLSVIhabitats = ConnectieLSVIhabitats)

    SoortengroepIDs <- Selectiegegevens %>%
      select(.data$TaxongroepId) %>%
      distinct() %>%
      filter(!is.na(.data$TaxongroepId)) %>%
      summarise(SoortengroepIDs = paste(.data$TaxongroepId, collapse = ","))
    
    if (SoortengroepIDs$SoortengroepIDs == "") {
      stop("Voor de opgegeven argumenten is er geen soortenlijst")
    }
    
    Soortenlijst <-
      geefSoortenlijstVoorIDs(
        Soortengroeplijst = SoortengroepIDs$SoortengroepIDs,
        Soortenlijsttype = Soortenlijsttype,
        ConnectieLSVIhabitats
      )

    #soortgegevens aan selectiegegevens plakken
    SoortenlijstSelectie <- Selectiegegevens %>%
      left_join(
        Soortenlijst,
        by = ("TaxongroepId")
      )

    return(SoortenlijstSelectie)

  }
