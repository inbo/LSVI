#' @title Genereert soortenlijst(en) LSVI op basis van de opgegeven parameters
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en
#' Nederlandse namen) die gebruikt worden voor de bepaling van de Lokale Staat
#' van Instandhouding van de opgegeven parameters.
#' In feite genereert ze een tabel met velden `Versie`, `Habitattype`,
#' `Habitatsubtype`, `WetNaam`, `WetNaamKort` en `NedNaam` en evt. `Criterium`,
#' `Indicator` en/of `Beschrijving` waarin de
#' gespecifieerde parameters uitgeselecteerd zijn en waar voor andere
#' parameters alle waarden uit de databank weergegeven zijn.
#'
#' Voor de vorm van de soortenlijst zijn er meerdere opties: een soortenlijst
#' met alle soorten per habitat(sub)type, ofwel gegroepeerd per criterium,
#' indicator of voorwaarde.  Dit kan opgegeven worden in de parameter
#' Taxonlijstniveau.
#'
#' @template Zoekparameters
#'
#' @inheritParams selecteerIndicatoren
#' @param Taxonlijstniveau Geeft aan op welk niveau de soortenlijst gegroepeerd
#' is (en welke niveaus weergegeven worden in de soortenlijst), de mogelijke
#' waarden zijn "habitattype", "criterium", "indicator" en "voorwaarde".
#' Default is "habitattype".
#' @param Taxonlijsttype `r lifecycle::badge("deprecated")`
#' `Taxonlijsttype = "alle"` wordt niet meer ondersteund; deze functie zal
#' altijd de soortenlijsten weergeven zoals in de habitatfiche (na herziening
#' van de soortafhandeling bevat het package geen volledige taxonomische
#' lijsten meer en is die functionaliteit ook overbodig geworden)
#'
#' @return Deze functie geeft een tabel met velden `Versie`, `Habitattype`,
#' `Habitatsubtype`, `Criterium`, `Indicator`, evt. `Beschrijving`, `WetNaam`,
#' `WetNaamKort` en `NedNaam` (waarbij `Beschrijving` een omschrijving is voor
#' een groep van taxa binnen eenzelfde indicator).  `WetNaam` is de volledige
#' Latijnse naam inclusief auteursnaam, `WetNaamKort` geeft de verkorte naam
#' zonder auteursnaam.
#' Daarnaast heeft de tabel ook de velden `GbifUsageKey` (unieke ID van Gbif)
#' en `Rank` (niveau van taxon en `GbifUsageKey`) die bij de berekeningen
#' gebruikt worden om de taxa van de opname te koppelen.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' maakConnectiePool()
#' geefSoortenlijst(Habitattype = "4030")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% select distinct filter group_by summarise ungroup
#' mutate left_join rename
#' @importFrom rlang .data
#' @importFrom lifecycle deprecated is_present
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
           Taxonlijsttype = deprecated(),
           ConnectieLSVIhabitats = NULL) {

    if (is_present(Taxonlijsttype)) {
      extra_tekst <- ""
      if (Taxonlijsttype == "alle") {
        extra_tekst <-
          " Het is niet meer mogelijk om alle taxa weer te geven die vallen onder de lijsten van de habitatfiches omdat deze info niet meer aanwezig is in het package. De uitvoer bevat enkel de soorten van de LSVI-fiche, geen onderliggende soorten." #nolint: line_length_linter
      }
      warning(
        sprintf(
          "Argument Taxonlijsttype van functie geefSoortenlijst() wordt niet meer ondersteund.%s", #nolint: line_length_linter
          extra_tekst
        )
      )
    }

    if (is.null(ConnectieLSVIhabitats)) {
      if (exists("ConnectiePool")) {
        ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
      }
    }
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint: line_length_linter
    )
    match.arg(Taxonlijstniveau)

    if (Taxonlijstniveau[1] != "voorwaarde") {
      Selectiegegevens <-
        selecteerIndicatoren(
          Versie = Versie,
          Habitatgroep = Habitatgroep,
          Habitattype = Habitattype,
          Criterium = Criterium,
          Indicator = Indicator,
          ConnectieLSVIhabitats = ConnectieLSVIhabitats
        )
    } else {
      Selectiegegevens <-
        geefInvoervereisten(
          Versie = Versie,
          Habitatgroep = Habitatgroep,
          Habitattype = Habitattype,
          Criterium = Criterium,
          Indicator = Indicator,
          ConnectieLSVIhabitats = ConnectieLSVIhabitats
        ) %>%
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
          Taxonlijsttype = deprecated(),
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
          "WetNaamKort", "TaxonType",
          "GbifUsageKey", "Rank"
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
