#' @title Genereert soorten(groep)lijst(en) LSVI op basis van TaxongroepID
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en
#' Nederlandse namen) uit de databank met de criteria en indicatoren voor de
#' bepaling van de Lokale Staat van Instandhouding.  Het is in feite een
#' hulpfunctie die voor verschillende andere functies gebruikt wordt en die de
#' complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een
#' opgegeven TaxongroepId (en in die zin iets minder gebruiksvriendelijk is).
#' Voor een selectie van soortenlijsten op basis van specifieke parameters is
#' de functie geefSoortenlijst() een beter alternatief.
#'
#' Deze functie geeft voor de gespecifieerde taxongroepen per groep
#' een lijst van alle taxa zoals ze in de LSVI-habitatfiche vermeld zijn
#' (genusniveau, soortniveau, subsoort,...) en ook de rank van de taxa.
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams geefSoortenlijst
#' @param Taxongroeplijst string waarin de TaxongroepId's na elkaar weergegeven
#' worden, gescheiden door een komma.  Eventueel mag dit ook een vector zijn
#' van TaxongroepId's.
#'
#' @return Deze functie geeft een tabel met velden TaxongroepId, evt.
#' Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een
#' omschrijving is voor een groep van taxons binnen eenzelfde indicator).
#' WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort
#' geeft de verkorte naam zonder auteursnaam.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' maakConnectiePool()
#' geefSoortenlijstVoorIDs("434,88,565")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% mutate filter distinct
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that noNA is.string
#' @importFrom lifecycle deprecated is_present
#'
#'
geefSoortenlijstVoorIDs <-
  function(Taxongroeplijst,
           Taxonlijsttype = deprecated(),
           ConnectieLSVIhabitats = NULL) {
    
    if (is_present(Taxonlijsttype)) {
      extra_tekst <- ""
      if (Taxonlijsttype == "alle") {
        extra_tekst <-
          " Het is niet meer mogelijk om alle taxa weer te geven die vallen onder de lijsten van de habitatfiches omdat deze info niet meer aanwezig is in het package. De uitvoer bevat enkel de soorten van de LSVI-fiche, geen onderliggende soorten."
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
    assert_that(is.character(Taxongroeplijst))
    if (!is.string(Taxongroeplijst)) {
      Taxongroeplijst <- paste(Taxongroeplijst, collapse = ",")
    }
    assert_that(is.string(Taxongroeplijst))
    assert_that(noNA(Taxongroeplijst))
    if (!grepl("^([[:digit:]]+,)*[[:digit:]]+$", Taxongroeplijst)) {
      stop("Taxongroeplijst bestaat niet uit een reeks getallen gescheiden door een komma") #nolint: line_length_linter
    }

    QueryGroepen <-
      sprintf(
        "WITH Groepen
        AS
        (
          SELECT Tg.Id AS TaxongroepId,
            Tg.Id AS TaxonsubgroepId
          FROM Taxongroep Tg
          WHERE Tg.Id in (%s)
        UNION ALL
          SELECT Groepen.TaxongroepId,
            Tg2.Id AS TaxonsubgroepId
          FROM Groepen
            INNER JOIN TaxongroepTaxongroep AS TgTg
            ON Groepen.TaxonsubgroepId = TgTg.TaxongroepParentId
          INNER JOIN Taxongroep Tg2
          ON TgTg.TaxongroepChildId = Tg2.Id
          WHERE TgTg.TaxongroepChildId > 0
        )",
        Taxongroeplijst
      )


    QueryLSVIfiche <-
      "
      SELECT Groepen.TaxongroepId,
        Groepen.TaxonsubgroepId,
        cast(Tg.Omschrijving AS nvarchar(90)) AS Omschrijving,
        Taxon.Id AS TaxonId,
        Taxon.NbnTaxonVersionKey,
        Taxon.FloraNaamWetenschappelijk AS WetNaam,
        Taxon.FloraNaamNederlands As NedNaam,
        Taxon.GbifUsageKey,
        Taxon.GbifAcceptedUsageKey,
        Taxon.Rank,
        TaxonType.Naam AS TaxonType,
        ts.CanonicalNameWithMarker AS WetNaamKort
      FROM Groepen
        INNER JOIN Taxongroep Tg
        ON Groepen.TaxonsubgroepId = Tg.Id
        INNER JOIN TaxongroepTaxon TgT
        ON Groepen.TaxonsubgroepId = TgT.TaxongroepId
        INNER JOIN Taxon
        ON TgT.TaxonId = Taxon.Id
        INNER JOIN TaxonType
        ON Taxon.TaxonTypeId = TaxonType.Id
        INNER JOIN TaxonSynoniem ts
        ON Taxon.Id = ts.TaxonId
      WHERE Taxon.NbnTaxonVersionKey = ts.NbnTaxonVersionKey;"



    Soortenlijst <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        paste(QueryGroepen, QueryLSVIfiche, sep = "")
      ) %>%
      distinct()

    return(Soortenlijst)
  }
