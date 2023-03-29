#' @title Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en
#' Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat
#' van Instandhouding van de opgegeven parameters.  In feite genereert ze een
#' tabel met velden Versie, Habitattype, Habitatsubtype, WetNaam, WetNaamKort
#' en NedNaam en evt. Criterium, Indicator en/of Beschrijving waarin de
#' gespecificeerde parameters uitgeselecteerd zijn en waar voor andere
#' parameters alle waarden uit de databank weergegeven zijn.
#'
#' Voor de vorm van de soortenlijst zijn er meerdere opties: een soortenlijst
#' met alle soorten per habitat(sub)type, ofwel gegroepeerd per criterium,
#' indicator of voorwaarde.  Dit kan opgegeven worden in de parameter
#' Taxonlijstniveau.
#'
#' Ook voor de weergave van de taxa zijn 2 opties: de taxa weergeven zoals in
#' de habitatfiches (op soortniveau, genusniveau of hoger niveau, zoals het in
#' de habitatfiches vermeld is) of alle taxa op lagere niveaus ook weergeven en
#' dus bij soortengroepen alle mogelijke soorten van deze groep weergeven.
#' Deze opties kunnen opgegeven worden in de parameter Taxonlijsttype.
#'
#' @template Zoekparameters
#'
#' @inheritParams selecteerIndicatoren
#' @param Taxonlijstniveau Geeft aan op welk niveau de soortenlijst gegroepeerd
#' is (en welke niveaus weergegeven worden in de soortenlijst), de mogelijke
#' waarden zijn 'habitattype', 'criterium', 'indicator' en 'voorwaarde'.
#' Default is 'habitattype'.
#' @param Taxonlijsttype "LSVIfiche" betekent dat de taxonlijst van de
#' habitatfiche wordt overgenomen, "alle" betekent dat alle soorten en alle
#' taxonomische groepen worden weergegeven die volledig in de groepen vallen
#' die aan de parameters voldoen.
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype,
#' Habitatsubtype, Criterium, Indicator, evt. Beschrijving, WetNaam,
#' WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een
#' groep van taxa binnen eenzelfde indicator).  WetNaam is de volledige
#' Latijnse naam inclusief auteursnaam, WetNaamKort geeft de verkorte naam
#' zonder auteursnaam.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' maakConnectiePool()
#' geefSoortenlijst(Habitattype = "4030", Taxonlijsttype = "LSVIfiche")
#' geefSoortenlijst(Habitattype = "4030", Taxonlijsttype = "alle")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% select distinct filter group_by summarise ungroup
#' mutate_ left_join rename
#' @importFrom rlang .data
#'
#'
geefSoortenlijst <-
  function(Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           Criterium = "alle",
           Indicator = "alle",
           Taxonlijstniveau =
             c("habitattype", "criterium", "indicator", "voorwaarde"),
           Taxonlijsttype = c("LSVIfiche", "alle"),
           ConnectieLSVIhabitats = NULL) {

    if (is.null(ConnectieLSVIhabitats)) {
      if (exists("ConnectiePool")) {
        ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
      }
    }
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint
    )
    match.arg(Taxonlijstniveau)
    match.arg(Taxonlijsttype)

    if (Taxonlijstniveau[1] != "voorwaarde") {
      Selectiegegevens <-
        selecteerIndicatoren(
          Versie = Versie,
          Habitatgroep = Habitatgroep,
          Habitattype = Habitattype,
          Criterium = Criterium,
          Indicator = Indicator,
          ConnectieLSVIhabitats = ConnectieLSVIhabitats)
    } else {
      Selectiegegevens <-
        geefInvoervereisten(
          Versie = Versie,
          Habitatgroep = Habitatgroep,
          Habitattype = Habitattype,
          Criterium = Criterium,
          Indicator = Indicator,
          ConnectieLSVIhabitats = ConnectieLSVIhabitats) %>%
        select(
          "Versie", "Habitattype", "Habitatsubtype",
          "Criterium", "Indicator", "Beoordeling",
          "Kwaliteitsniveau", "Voorwaarde", "TaxongroepId"
        ) %>%
        distinct()
    }

    SoortengroepIDs <- Selectiegegevens %>%
      select("TaxongroepId") %>%
      distinct() %>%
      filter(!is.na(.data$TaxongroepId)) %>%
      summarise(SoortengroepIDs = paste(.data$TaxongroepId, collapse = ","))

    if (SoortengroepIDs$SoortengroepIDs == "") {
      warning("Voor de opgegeven argumenten is er geen soortenlijst")
      SoortenlijstSelectie <- Selectiegegevens %>%
        mutate(
          TaxonsubgroepId = NA,
          Omschrijving = NA,
          Id = NA,
          TaxonId = NA,
          SubTaxonId = NA,
          NbnTaxonVersionKey = NA,
          WetNaam = NA,
          NedNaam = NA,
          WetNaamKort = NA,
          TaxonType = NA
        )
    } else {
      Soortenlijst <-
        geefSoortenlijstVoorIDs(
          Taxongroeplijst = SoortengroepIDs$SoortengroepIDs,
          Taxonlijsttype = Taxonlijsttype,
          ConnectieLSVIhabitats
        )

      #soortgegevens aan selectiegegevens plakken
      SoortenlijstSelectie <- Selectiegegevens %>%
        left_join(
          Soortenlijst,
          by = ("TaxongroepId")
        )
    }

    if (Taxonlijstniveau[1] != "voorwaarde") {
      SoortenlijstSelectie <- SoortenlijstSelectie %>%
        select(
          "Versie", "Habitattype", "Habitatsubtype",
          "Criterium", "Indicator", "TaxongroepId",
          "Omschrijving",
          "NbnTaxonVersionKey", "WetNaam", "NedNaam",
          "WetNaamKort", "TaxonType"
        ) %>%
        distinct()
    }

    if (Taxonlijstniveau[1] == "criterium") {
      SoortenlijstSelectie <- SoortenlijstSelectie %>%
        select(-"Indicator") %>%
        filter(!is.na(.data$NbnTaxonVersionKey)) %>%
        distinct()
    }
    if (Taxonlijstniveau[1] == "habitattype") {
      SoortenlijstSelectie <- SoortenlijstSelectie %>%
        select(-"Indicator", -"Criterium") %>%
        filter(!is.na(.data$NbnTaxonVersionKey)) %>%
        distinct()
    }

    return(SoortenlijstSelectie)

  }
