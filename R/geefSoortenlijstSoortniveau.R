#' @title Genereert soortenlijst(en) LSVI op basis van SoortengroepID
#'
#' @description Deze functie genereert soortenlijsten (met wetenschappelijke en Nederlandse namen) uit de databank met de criteria en indicatoren voor de bepaling van de Lokale Staat van Instandhouding.  Het is in feite een hulpfunctie die voor verschillende andere functies gebruikt wordt en die de complexe zoekfunctie in de tabellen met soorten uitvoert op basis van een opgegeven SoortengroepID (en in die zin iets minder gebruiksvriendelijk is).  Voor een selectie van soortenlijsten op basis van specifieke parameters is de functie geefSoortenlijst() een beter alternatief.
#' 
#' Deze functie geeft voor de gespecifieerde soortengroepen per soortengroep een lijst van alle soorten die hieronder vallen.  Dit betekent dat voor de in de LSVI vermelde genera en soortengroepen als helofieten alle soorten vermeld worden die tot deze groepen behoren.  Voor een lijst van soort(groep)en op het niveau vermeld in de LSVI, wordt beter de functie geefSoortenlijstInvoerniveau() gebruikt.
#'
#' @inheritParams selecteerIndicatoren
#' @param Soortengroeplijst string waarin de SoortengroepID's na elkaar weergegeven worden, gescheiden door een komma
#' @param Soortenlijsttype "Soortniveau" betekent dat alle soorten worden weergegeven van de opgegeven soortgroepen, "alle" betekent dat alle soorten en alle taxonomische en morfologische groepen worden weergegeven die volledig in de opgegeven soortgroepen vallen (die aan de parameters voldoen (dus hier wordt de lijst uitgebreid met hogere taxonomische en morfologische groepen).
#'
#' @return Deze functie geeft een tabel met velden SoortengroepID, evt. Beschrijving, WetNaam, WetNaamKort en NedNaam (waarbij Beschrijving een omschrijving is voor een groep van soorten binnen eenzelfde indicator).  WetNaam is de volledige Latijnse naam inclusief auteursnaam, WetNaamKort bevat enkel genusnaam en soortnaam (zonder auteursnaam).
#' 
#' @examples
#' geefSoortenlijstSoortniveau("139,142,370,371")
#' geefSoortenlijstSoortniveau("139,142,370,371","Soortniveau")
#'
#' @export
#'
#' @importFrom dplyr %>% bind_rows mutate_ filter_ distinct_
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom assertthat assert_that noNA is.string
#'
#'
geefSoortenlijstSoortniveau <-
  function(Soortengroeplijst,
           Soortenlijsttype = c("alle", "Soortniveau"),
           ConnectieLSVIhabitats = connecteerMetLSVIdb()){

    assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))
    assert_that(is.string(Soortengroeplijst))
    assert_that(noNA(Soortengroeplijst))
    if (!grepl("^([[:digit:]]+,)*[[:digit:]]+$", Soortengroeplijst)) {
      stop("Soortengroeplijst bestaat niet uit een reeks getallen gescheiden door een komma") #nolint
    }
    match.arg(Soortenlijsttype)

    if (Soortenlijsttype[1] == "Soortniveau") {
      query <-
        sprintf(
          "WITH Soortengroepniveau
          AS
          (
            SELECT Soortengroep.Id as SoortengroepID,
                 SoortengroepSoort.SoortensubgroepID,
                 SoortengroepSoort.SoortID, 0 AS Niveau
            FROM Soortengroep INNER JOIN SoortengroepSoort
              ON Soortengroep.Id = SoortengroepSoort.SoortengroepID
            WHERE Soortengroep.Id in (%s)
            UNION ALL
            SELECT Soortengroepniveau.SoortengroepID,
                 ss2.SoortensubgroepID, ss2.SoortID, Niveau + 1
            FROM
              (Soortengroep AS s2 INNER JOIN SoortengroepSoort AS ss2
                ON s2.Id = ss2.SoortengroepID)
              INNER JOIN Soortengroepniveau
                ON s2.Id = Soortengroepniveau.SoortensubgroepID
          )
          SELECT Soortengroepniveau.SoortengroepID,
                 Soort.WetNaam, Soort.NedNaam
          FROM Soortengroepniveau
          INNER JOIN Soort ON Soortengroepniveau.SoortID = Soort.Id",
          Soortengroeplijst
        )

      Soortenlijst <-
        sqlQuery(ConnectieLSVIhabitats, query, stringsAsFactors = FALSE)

    } else if (Soortenlijsttype[1] == "alle") {
      query <-
        sprintf(
          "WITH Soortengroepniveau
          AS
          (
            SELECT Soortengroep.Id as SoortengroepID,
                 SoortengroepSoort.SoortensubgroepID,
                 SoortengroepSoort.SoortID, 0 AS Niveau
            FROM Soortengroep INNER JOIN SoortengroepSoort
              ON Soortengroep.Id = SoortengroepSoort.SoortengroepID
            WHERE Soortengroep.Id in (%s)
            UNION ALL
            SELECT Soortengroepniveau.SoortengroepID,
                 ss2.SoortensubgroepID, ss2.SoortID, Niveau + 1
            FROM
              (Soortengroep AS s2 INNER JOIN SoortengroepSoort AS ss2
                ON s2.Id = ss2.SoortengroepID)
              INNER JOIN Soortengroepniveau
                ON s2.Id = Soortengroepniveau.SoortensubgroepID
          )
          SELECT Soortengroepniveau.SoortengroepID,
                 Soortengroep.Naam AS NedNaam_groep,
                 Soortengroep.WetNaam AS WetNaam_groep,
                 Soortengroeptype.Omschrijving AS Soortengroeptype,
                 Soort.WetNaam, Soort.NedNaam,
                 Soort.NBNTaxonVersionKey,
                 Taxontype.Naam AS Taxontype
          FROM Soortengroepniveau
          LEFT JOIN 
            (Soort INNER JOIN Taxontype ON Soort.TaxonTypeId = Taxontype.Id)
            ON Soortengroepniveau.SoortID = Soort.Id
          LEFT JOIN
            (Soortengroep INNER JOIN Soortengroeptype
              ON Soortengroep.SoortengroeptypeID = Soortengroeptype.Id)
            ON Soortengroepniveau.SoortensubgroepID = Soortengroep.Id",
          Soortengroeplijst
        )

      Soortenlijst <-
        sqlQuery(ConnectieLSVIhabitats, query, stringsAsFactors = FALSE)

      Soortenlijst <- Soortenlijst %>%
        mutate_(
          NedNaam = ~ifelse(is.na(NedNaam) & Soortengroeptype != "Conceptueel",
                            NedNaam_groep,
                            NedNaam),
          WetNaam = ~ifelse(is.na(WetNaam) & Soortengroeptype != "Conceptueel",
                            WetNaam_groep,
                            WetNaam),
          NedNaam_groep = ~NULL,
          WetNaam_groep = ~NULL,
          Soortengroeptype = ~NULL
        )
    }


    #Kolommen met WetNaam (uit tabellen Soortengroep en Soort) samenvoegen,
    #id voor NedNaam, en een kolom WetNaamKort toevoegen
    Soortenlijst <- Soortenlijst %>%
      distinct_() %>%
      filter_(
        ~!is.na(WetNaam) | !is.na(NedNaam)
      ) %>%
      mutate_(
        WetNaamKort = ~
          gsub(
            pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
            replacement = "\\1 \\2",
            x = WetNaam
          )
      )

    return(Soortenlijst)
  }
