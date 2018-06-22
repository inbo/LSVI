#' @title Genereert soorten(groep)lijst(en) LSVI op basis van SoortengroepID
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) uit de databank met de criteria en indicatoren voor de bepaling van de Lokale Staat van Instandhouding.  Het is in feite een hulpfunctie die voor verschillende andere functies gebruikt wordt en die de complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een opgegeven SoortengroepID (en in die zin iets minder gebruiksvriendelijk is).  Voor een selectie van soortenlijsten op basis van specifieke parameters is de functie geefSoortenlijst() een beter alternatief.
#' 
#' Deze functie geeft standaard voor de gespecifieerde soortengroepen per soortengroep een lijst van alle taxa zoals ze in de LSVI-habitatfiche vermeld zijn (genusniveau, soortniveau, subsoort,...).  Op basis van de parameter soortenlijsttype kan ook gekozen worden om een volledige lijst te geven van deze taxa en alle taxa die hieronder vallen (en opgenomen zijn in de onderliggende databank).
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams geefSoortenlijst
#' @param Soortengroeplijst string waarin de SoortengroepID's na elkaar weergegeven worden, gescheiden door een komma.  Eventueel mag dit ook een vector zijn van SoortengroepID's.
#'
#' @return Deze functie geeft een tabel met velden SoortengroepID, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#' 
#' @examples
#' geefSoortenlijstVoorIDs("98,227,484,552,726")
#' geefSoortenlijstVoorIDs("98,227,484,552,726","alle")
#'
#' @export
#'
#' @importFrom dplyr %>% mutate filter distinct
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that noNA is.string
#'
#'
geefSoortenlijstVoorIDs <-
  function(Soortengroeplijst,
           Soortenlijsttype = c("LSVIfiche", "alle"),
           ConnectieLSVIhabitats = ConnectiePool){

    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )
    assert_that(is.character(Soortengroeplijst))
    if (!is.string(Soortengroeplijst)) {
      Soortengroeplijst <- paste(Soortengroeplijst, collapse = ",")
    }
    assert_that(is.string(Soortengroeplijst))
    assert_that(noNA(Soortengroeplijst))
    if (!grepl("^([[:digit:]]+,)*[[:digit:]]+$", Soortengroeplijst)) {
      stop("Soortengroeplijst bestaat niet uit een reeks getallen gescheiden door een komma") #nolint
    }
    match.arg(Soortenlijsttype)

    QueryGroepen <-
      sprintf(
        "WITH Groepen
        AS
        (
          SELECT Tg.Id AS TaxongroepId,
            Tg.Id AS TaxonsubgroepId,
            Tg.Omschrijving
          FROM Taxongroep Tg
          WHERE Tg.Id in (%s)
        UNION ALL
          SELECT Groepen.TaxongroepId,
            Tg2.Id AS TaxonsubgroepId,
        	Tg2.Omschrijving
          FROM Groepen 
            INNER JOIN TaxongroepTaxongroep AS TgTg
            ON Groepen.TaxonsubgroepId = TgTg.TaxongroepParentId
        	INNER JOIN Taxongroep Tg2
        	ON TgTg.TaxongroepChildId = Tg2.Id
          WHERE TgTg.TaxongroepChildId > 0
        )",
        Soortengroeplijst
      )

    QueryTaxa <-
      ",
      Taxonlijn
      AS
      (
        SELECT Tx.Id AS TaxonId,
          Tx.Id AS SubTaxonId,
          Tx.NbnTaxonVersionKey,
      	Tx.FloraNaamWetenschappelijk,
      	Tx.FloraNaamNederlands,
        Tx.NbnNaam,
      	Tx.TaxonTypeId
        FROM Taxon Tx
      UNION ALL
        SELECT Taxonlijn.TaxonId,
          Tx2.Id AS SubTaxonId,
      	Tx2.NbnTaxonVersionKey,
      	Tx2.FloraNaamWetenschappelijk,
      	Tx2.FloraNaamNederlands,
        Tx2.NbnNaam,
      	Tx2.TaxonTypeId
        FROM Taxonlijn 
          INNER JOIN TaxonTaxon AS TxTx
          ON Taxonlijn.SubTaxonId = TxTx.TaxonParentId
      	INNER JOIN Taxon Tx2
      	ON TxTx.TaxonChildId = Tx2.Id
        WHERE TxTx.TaxonChildId > 0
      )"
    
    QueryLSVIfiche <-
      "
      SELECT Groepen.TaxongroepId,
        Groepen.TaxonsubgroepId,
        Groepen.Omschrijving,
        Taxon.Id,
        Taxon.NbnTaxonVersionKey,
        Taxon.FloraNaamWetenschappelijk AS WetNaam,
        Taxon.FloraNaamNederlands As NedNaam,
        Taxon.NbnNaam AS WetNaamKort,
        TaxonType.Naam AS TaxonType
      FROM Groepen 
        INNER JOIN TaxongroepTaxon TgT 
        on Groepen.TaxongroepId = TgT.TaxongroepId
        INNER JOIN Taxon
        ON TgT.TaxonId = Taxon.Id
        INNER JOIN TaxonType
        ON Taxon.TaxonTypeId = TaxonType.Id;"
    
    QueryAlleTaxa <-
      "
      SELECT Groepen.TaxongroepId,
        Groepen.TaxonsubgroepId,
        Groepen.Omschrijving,
        Taxonlijn.TaxonId,
        Taxonlijn.SubTaxonId,
        Taxonlijn.NbnTaxonVersionKey,
        Taxonlijn.FloraNaamWetenschappelijk AS WetNaam,
        Taxonlijn.FloraNaamNederlands As NedNaam,
        Taxonlijn.NbnNaam AS WetNaamKort,
        TaxonType.Naam AS TaxonType
      FROM Groepen 
        INNER JOIN TaxongroepTaxon TgT 
        on Groepen.TaxongroepId = TgT.TaxongroepId
        INNER JOIN Taxonlijn
        ON TgT.TaxonId = Taxonlijn.TaxonId
        INNER JOIN TaxonType
        ON Taxonlijn.TaxonTypeId = TaxonType.Id
      ORDER BY Groepen.TaxongroepId, Groepen.TaxonsubgroepId,
        Taxonlijn.TaxonId;"
    
    if (Soortenlijsttype[1] == "LSVIfiche") {
      Soortenlijst <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          paste(QueryGroepen, QueryLSVIfiche, sep = "")
        )

    } else if (Soortenlijsttype[1] == "alle") {
      Soortenlijst <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          paste(QueryGroepen, QueryTaxa, QueryAlleTaxa, sep = "")
        )
    }

    return(Soortenlijst)
  }
