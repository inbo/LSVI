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
#' Deze functie geeft standaard voor de gespecifieerde taxongroepen per groep
#' een lijst van alle taxa zoals ze in de LSVI-habitatfiche vermeld zijn
#' (genusniveau, soortniveau, subsoort,...).  Op basis van de parameter
#' soortenlijsttype kan ook gekozen worden om een volledige lijst te geven van
#' deze taxa en alle taxa die hieronder vallen (en opgenomen zijn in de
#' onderliggende databank).
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
#' geefSoortenlijstVoorIDs("434,88,565","alle")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% mutate filter distinct
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that noNA is.string
#'
#'
geefSoortenlijstVoorIDs <-
  function(Taxongroeplijst,
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
    assert_that(is.character(Taxongroeplijst))
    if (!is.string(Taxongroeplijst)) {
      Taxongroeplijst <- paste(Taxongroeplijst, collapse = ",")
    }
    assert_that(is.string(Taxongroeplijst))
    assert_that(noNA(Taxongroeplijst))
    if (!grepl("^([[:digit:]]+,)*[[:digit:]]+$", Taxongroeplijst)) {
      stop("Taxongroeplijst bestaat niet uit een reeks getallen gescheiden door een komma") #nolint
    }
    match.arg(Taxonlijsttype)

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

    QueryTaxa <-
      ",
    UniekeTaxa
    AS
    (
      SELECT DISTINCT TgT.TaxonId
      FROM Groepen INNER JOIN TaxongroepTaxon TgT
      ON Groepen.TaxonsubgroepId = TgT.TaxongroepId
    ),
    Taxonlijn
    AS
    (
      SELECT Tx.Id AS AncestorTaxonId,
        Tx.Id AS ParentTaxonId,
        Tx.Id AS SubTaxonId
      FROM Taxon Tx
      WHERE Tx.Id IN (SELECT TaxonId FROM UniekeTaxa)
    UNION ALL
      SELECT Taxonlijn.AncestorTaxonId,
        Txtx.TaxonParentId AS ParentTaxonId,
        Tx2.Id AS SubTaxonId
      FROM Taxonlijn
        INNER JOIN TaxonTaxon AS TxTx
          ON Taxonlijn.SubTaxonId = TxTx.TaxonParentId
          AND Taxonlijn.ParentTaxonId != TxTx.TaxonChildId
        INNER JOIN Taxon Tx2
          ON TxTx.TaxonChildId = Tx2.Id
      WHERE TxTx.TaxonChildId > 0
    )"

    QueryLSVIfiche <-
      "
      SELECT Groepen.TaxongroepId,
        Groepen.TaxonsubgroepId,
        cast(Tg.Omschrijving AS nvarchar(90)) AS Omschrijving,
        Taxon.Id,
        Taxon.NbnTaxonVersionKey,
        Taxon.FloraNaamWetenschappelijk AS WetNaam,
        Taxon.FloraNaamNederlands As NedNaam,
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

    QueryAlleTaxa <-
      "
      SELECT Groepen.TaxongroepId,
        Groepen.TaxonsubgroepId,
        cast(Tg.Omschrijving AS nvarchar(90)) AS Omschrijving,
        Taxonlijn.AncestorTaxonId AS TaxonId,
        Taxonlijn.SubTaxonId,
        Taxon.NbnTaxonVersionKey,
        Taxon.FloraNaamWetenschappelijk AS WetNaam,
        Taxon.FloraNaamNederlands As NedNaam,
        TaxonType.Naam AS TaxonType,
        ts.CanonicalNameWithMarker AS WetNaamKort
      FROM Groepen
        INNER JOIN Taxongroep Tg
        ON Groepen.TaxonsubgroepId = Tg.Id
        INNER JOIN TaxongroepTaxon TgT
        ON Groepen.TaxonsubgroepId = TgT.TaxongroepId
        INNER JOIN Taxonlijn
        ON TgT.TaxonId = Taxonlijn.AncestorTaxonId
        INNER JOIN Taxon
        ON Taxonlijn.SubTaxonId = Taxon.Id
        INNER JOIN TaxonType
        ON Taxon.TaxonTypeId = TaxonType.Id
        INNER JOIN TaxonSynoniem Ts
          ON Taxon.Id = Ts.TaxonId
      WHERE Taxon.NbnTaxonVersionKey = Ts.NbnTaxonVersionKey
      ORDER BY Groepen.TaxongroepId, Groepen.TaxonsubgroepId,
        Taxonlijn.AncestorTaxonId;"

    if (Taxonlijsttype[1] == "LSVIfiche") {
      Soortenlijst <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          paste(QueryGroepen, QueryLSVIfiche, sep = "")
        ) %>%
        distinct()

    } else if (Taxonlijsttype[1] == "alle") {
      Soortenlijst <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          paste(QueryGroepen, QueryTaxa, QueryAlleTaxa, sep = "")
        ) %>%
        distinct()
    }

    return(Soortenlijst)
  }
