#' @title Genereert soorten(groep)lijst(en) LSVI op basis van TaxonGroepCode
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en
#' Nederlandse namen) uit de databank met de criteria en indicatoren voor de
#' bepaling van de Lokale Staat van Instandhouding.  Het is in feite een
#' hulpfunctie die voor verschillende andere functies gebruikt wordt en die de
#' complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een
#' opgegeven `TaxonGroepCode` (en in die zin iets minder gebruiksvriendelijk
#' is).
#' Voor een selectie van soortenlijsten op basis van specifieke parameters is
#' de functie `geefSoortenlijst()` een beter alternatief.
#'
#' Deze functie geeft voor de gespecifieerde taxongroepen per groep
#' een lijst van alle taxa zoals ze in de LSVI-habitatfiche vermeld zijn
#' (genusniveau, soortniveau, subsoort,...) en ook de rank van de taxa.
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams geefSoortenlijst
#' @param Taxongroeplijst string waarin de `TaxonGroepCode`'s (tussen enkele
#' quotes) na elkaar
#' weergegeven worden, gescheiden door een komma.
#' Eventueel mag dit ook een vector zijn van `TaxonGroepCode`'s.
#'
#' @return Deze functie geeft een tabel met velden `TaxonGroepCode`, evt.
#' `Beschrijving`, `WetNaam`, `WetNaamKort` en `NedNaam` (waarbij `Beschrijving`
#' een omschrijving is voor een groep van taxons binnen eenzelfde indicator).
#' `WetNaam` is de volledige Latijnse naam inclusief auteursnaam, `WetNaamKort`
#' geeft de verkorte naam zonder auteursnaam.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' maakConnectiePool()
#' geefSoortenlijstVoorIDs(
#'   "'CsLocal-0x1C4FE483F305B196626452A19DB5DCCCA4DB711D8C66D5058EC1',
#'   'CsLocal-0x4C127745A720F68BBF488A79F63323CC8FD9A273FD968F473869',
#'   'CsLocal-0x84DDA74D1AB0D4DAE0B83E47A049C7DEFCA4A24D38CAF4B73884',
#'   'Flora-0xCF33C29F19C96C449E694A1692812CF7C56616BF996B5A80DEAE'"
#' )
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
    assert_that(is.character(Taxongroeplijst))
    if (!is.string(Taxongroeplijst)) {
      Taxongroeplijst <- paste(Taxongroeplijst, collapse = "','")
    }
    if (!grepl("^'.*'$", Taxongroeplijst)) {
      Taxongroeplijst <- paste0("'", Taxongroeplijst, "'")
    }
    assert_that(is.string(Taxongroeplijst))
    assert_that(noNA(Taxongroeplijst))

    Query <-
      "SELECT tgt.TaxonGroepCode,
        t.WetNaam,
        ot.NaamNederlands AS NedNaam,
        t.canonicalName AS WetNaamKort,
        t.GbifUsageKey, t.Rank
      FROM TaxonGroepTaxon tgt
        LEFT JOIN Taxon t ON tgt.TaxonKey = t.GbifUsageKey
        LEFT JOIN ObservatieTaxon ot on t.GbifUsageKey = ot.GbifUsageKey
      WHERE tgt.TaxonGroepCode in (%s) AND ot.NaamNederlands IS NOT NULL
      GROUP BY tgt.TaxonGroepCode, t.WetNaam,
        ot.NaamNederlands, t.canonicalName, t.GbifUsageKey, t.Rank"

    Soortenlijst <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        sprintf(Query, Taxongroeplijst)
      ) %>%
      distinct()

    return(Soortenlijst)
  }
