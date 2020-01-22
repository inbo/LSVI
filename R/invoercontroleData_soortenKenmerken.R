#' Invoercontrole voor dataframe Data_soortenKenmerken
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en
#' om de hoofdscripts overzichtelijk te houden, maken we voor elke
#' invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze
#' wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund
#' worden (maar worden wel gerund als de functie waarin ze voorkomen,
#' aangeroepen wordt).  Ingeval van Data_soortenKenmerken is ook de omzetting
#' van soortnamen naar een NBNTaxonVersionKey en de omzettingen van bedekkingen
#' naar een interval opgenomen in de functie.
#'
#' @param Data_soortenKenmerken dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% filter n mutate select left_join bind_rows rename
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#'
#' @export
#'
invoercontroleData_soortenKenmerken <- #nolint
  function(Data_soortenKenmerken, ConnectieLSVIhabitats, LIJST) { #nolint
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )

    assert_that(inherits(Data_soortenKenmerken, "data.frame"))
    assert_that(has_name(Data_soortenKenmerken, "ID"))
    if (!is.character(Data_soortenKenmerken$ID)) {
      Data_soortenKenmerken$ID <- as.character(Data_soortenKenmerken$ID)
    }
    assert_that(has_name(Data_soortenKenmerken, "Kenmerk"))
    if (!is.character(Data_soortenKenmerken$Kenmerk)) {
      Data_soortenKenmerken$Kenmerk <-
        as.character(Data_soortenKenmerken$Kenmerk)
    }
    assert_that(has_name(Data_soortenKenmerken, "TypeKenmerk"))
    if (!is.character(Data_soortenKenmerken$TypeKenmerk)) {
      Data_soortenKenmerken$TypeKenmerk <-
        as.character(Data_soortenKenmerken$TypeKenmerk)
    }
    Data_soortenKenmerken$TypeKenmerk <-
      tolower(Data_soortenKenmerken$TypeKenmerk)
    assert_that(
      all(
        Data_soortenKenmerken$TypeKenmerk %in%
          c("studiegroep", "soort_nbn", "soort_latijn", "soort_nl", "doodhout")
      ),
      msg = "Data_soortenKenmerken$TypeKenmerk moet een van de volgende waarden zijn: studiegroep, soort_nbn, soort_latijn, soort_nl, doodhout" #nolint
    )
    assert_that(has_name(Data_soortenKenmerken, "Waarde"))
    if (!is.character(Data_soortenKenmerken$Waarde)) {
      Data_soortenKenmerken$Waarde <-
        as.character(Data_soortenKenmerken$Waarde)
    }
    assert_that(has_name(Data_soortenKenmerken, "Type"))
    if (!is.character(Data_soortenKenmerken$Type)) {
      Data_soortenKenmerken$Type <-
        as.character(Data_soortenKenmerken$Type)
    }
    Data_soortenKenmerken$Type <- str_to_sentence(Data_soortenKenmerken$Type)
    controleerInvoerwaarde(
      "Data_soortenKenmerken$Type", Data_soortenKenmerken$Type,
      "TypeVariabele", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )
    assert_that(has_name(Data_soortenKenmerken, "Invoertype"))
    if (!is.character(Data_soortenKenmerken$Invoertype)) {
      Data_soortenKenmerken$Invoertype <-
        as.character(Data_soortenKenmerken$Invoertype)
    }
    controleerInvoerwaarde(
      "Data_soortenKenmerken$Invoertype",
      Data_soortenKenmerken$Invoertype[
        !is.na(Data_soortenKenmerken$Invoertype)
      ],
      "Lijst", "Naam", ConnectieLSVIhabitats
    )
    assert_that(has_name(Data_soortenKenmerken, "Eenheid"))
    if (!is.character(Data_soortenKenmerken$Eenheid)) {
      Data_soortenKenmerken$Eenheid <-
        as.character(Data_soortenKenmerken$Eenheid)
    }
    GeldigeWaarden <-
      c(
        geefUniekeWaarden(
          "AnalyseVariabele",
          "Eenheid",
          ConnectieLSVIhabitats
        ),
        "Volume_ha",
        "Aantal_ha",
        "Grondvlak_ha"
      )

    if (
      !all(
         Data_soortenKenmerken$Eenheid %in% GeldigeWaarden
        )
      ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Eenheid komen overeen met waarden vermeld in de databank.") #nolint
    }

    assert_that(has_name(Data_soortenKenmerken, "Vegetatielaag"))
    if (!is.character(Data_soortenKenmerken$Vegetatielaag)) {
      Data_soortenKenmerken$Vegetatielaag <-
        as.character(tolower(Data_soortenKenmerken$Vegetatielaag))
    }
    controleerInvoerwaarde(
      "Data_soortenKenmerken$Vegetatielaag",
      Data_soortenKenmerken$Vegetatielaag[
        !is.na(Data_soortenKenmerken$Vegetatielaag)
      ],
      "StudieItem", "Waarde", ConnectieLSVIhabitats
    )


    # Omzettingen naar een bruikbare dataframe
    Kenmerken <- Data_soortenKenmerken    # naamsverandering!

    # Om naamsverandering in databank van GbifCanonicalNameWithMarker naar
    # CanonicalNameWithMarker op te vangen
    if (class(ConnectieLSVIhabitats)[1] == "Pool") {
      Klasse <-
        class(ConnectieLSVIhabitats$.__enclos_env__$private$createObject())[1]
    } else {
      Klasse <- class(ConnectieLSVIhabitats)[1]
    }

    if (Klasse == "Microsoft SQL Server") {
      QuerySoorten <-
        "SELECT TaxonSynoniem.FloraNaamNederlands AS NedNaam,
            TaxonSynoniem.GbifCanonicalNameWithMarker AS Canonicalname,
            Taxon.NbnTaxonVersionKey AS NBNTaxonVersionKey, Taxon.TaxonTypeId
        FROM TaxonSynoniem INNER JOIN Taxon
          ON TaxonSynoniem.TaxonId = Taxon.Id
        WHERE Taxon.NbnTaxonVersionKey IS NOT NULL"
    } else {
      QuerySoorten <-
        "SELECT TaxonSynoniem.FloraNaamNederlands AS NedNaam,
            TaxonSynoniem.CanonicalNameWithMarker AS Canonicalname,
            Taxon.NbnTaxonVersionKey AS NBNTaxonVersionKey, Taxon.TaxonTypeId
        FROM TaxonSynoniem INNER JOIN Taxon
          ON TaxonSynoniem.TaxonId = Taxon.Id
        WHERE Taxon.NbnTaxonVersionKey IS NOT NULL"
    }

    Taxonlijst <-
      dbGetQuery(ConnectieLSVIhabitats, QuerySoorten)

    berekenCanonicalname <- function(Soortenlijst) {
      if (length(Soortenlijst) == 0) {
        return(as.character("geenSoort"))
      } else {
        return(parseTaxonnaam(Soortenlijst))
      }
    }

    KenmerkenSoort <- Kenmerken %>%
      filter(tolower(.data$TypeKenmerk) == "soort_latijn") %>%
      mutate(
        Canonicalname = berekenCanonicalname(.data$Kenmerk)
      ) %>%
      left_join(
        Taxonlijst %>%
          select(
            .data$Canonicalname, .data$NBNTaxonVersionKey
          ) %>%
          distinct(),
        by = c("Canonicalname")
      ) %>%
      bind_rows(
        Kenmerken %>%
          filter(tolower(.data$TypeKenmerk) == "soort_nl") %>%
          left_join(
            Taxonlijst %>%
              select(
                .data$NedNaam, .data$NBNTaxonVersionKey
              ) %>%
              distinct(),
            by = c("Kenmerk" = "NedNaam")
          )
      )


    Fouten <- KenmerkenSoort %>%
      filter(is.na(.data$NBNTaxonVersionKey))
    if (nrow(Fouten) > 0) {
      warning(
        sprintf(
          "Volgende soortnamen zijn niet teruggevonden in de databank: %s.  Check de spelling en/of laat de auteursnaam weg bij genera.",  #nolint
          paste(unique(Fouten$Kenmerk), collapse = ", ")
        )
      )
    }

    Fouten <- Kenmerken %>%
      filter(tolower(.data$TypeKenmerk) == "soort_nbn") %>%
      mutate(
        Fout = !.data$Kenmerk %in% Taxonlijst$NBNTaxonVersionKey
      ) %>%
      filter(.data$Fout == TRUE)

    if (nrow(Fouten) > 0) {
      warning(
        sprintf(
          "Volgende NBNTaxonVersionKeys zijn niet teruggevonden in de databank: %s.  Check de juistheid hiervan als deze mogelijk relevant zijn voor de berekening.",  #nolint
          paste(unique(Fouten$Kenmerk), collapse = ", ")
        )
      )
    }

    Dubbels <- KenmerkenSoort %>%
      group_by(
        .data$ID, .data$NBNTaxonVersionKey, .data$Vegetatielaag, .data$Eenheid,
        .data$Canonicalname
      ) %>%
      summarise(Aantal = n()) %>%
      ungroup() %>%
      filter(.data$Aantal > 1)
    if (nrow(Dubbels) > 0) {
      Tekst <- Dubbels %>%
        inner_join(
          KenmerkenSoort,
          by = c("ID", "NBNTaxonVersionKey", "Vegetatielaag", "Eenheid",
                 "Canonicalname")
        ) %>%
        group_by(.data$ID, .data$Vegetatielaag) %>%
        summarise(
          Soorten = paste(unique(.data$Kenmerk), collapse = "', '")
        ) %>%
        ungroup() %>%
        mutate(
          TekstOpname =
            paste0(
              "Voor opname ", .data$ID, " is/zijn de soort(en) '",
              .data$Soorten, "' meermaals opgegeven voor de ",
              .data$Vegetatielaag, collapse = NULL
            )
        ) %>%
        summarise(
          Tekst = paste(.data$TekstOpname, collapse = "; ")
        )
      warning(Tekst$Tekst)
    }

    Synoniemen <- KenmerkenSoort %>%
      group_by(
        .data$ID, .data$NBNTaxonVersionKey, .data$Vegetatielaag, .data$Eenheid
      ) %>%
      summarise(Aantal = n()) %>%
      ungroup() %>%
      filter(.data$Aantal > 1)
    if (nrow(Synoniemen) > 0) {
      Synoniemen <- Synoniemen %>%
        inner_join(
          KenmerkenSoort,
          by = c("ID", "NBNTaxonVersionKey", "Vegetatielaag", "Eenheid")
        )
      LatijnEnNl <- Synoniemen %>%
        group_by(
          .data$ID, .data$NBNTaxonVersionKey, .data$Vegetatielaag,
          .data$Eenheid, .data$TypeKenmerk
        ) %>%
        summarise(Aantal = n()) %>%
        ungroup() %>%
        filter(.data$Aantal == 1)
      if (nrow(LatijnEnNl) > 0) {
        Tekst <- Synoniemen %>%
          group_by(.data$ID, .data$Vegetatielaag) %>%
          summarise(
            Soorten = paste(unique(.data$Kenmerk), collapse = "' / '")
          ) %>%
          ungroup() %>%
          mutate(
            TekstOpname =
              paste0(
                "Voor opname ", .data$ID, " zijn in de ", .data$Vegetatielaag,
                " zowel Nederlandse als Latijnse namen gebruikt voor de soort '", #nolint
                .data$Soorten, collapse = NULL
              )
          ) %>%
          summarise(
            Tekst = paste(.data$TekstOpname, collapse = "; ")
          )
        stop(Tekst$Tekst)
      } else {
        Tekst <- Synoniemen %>%
          group_by(.data$ID, .data$Vegetatielaag) %>%
          summarise(
            Soorten = paste(unique(.data$Kenmerk), collapse = "' en '")
          ) %>%
          ungroup() %>%
          mutate(
            TekstOpname =
              paste0(
                "Voor opname ", .data$ID, " zijn in de ", .data$Vegetatielaag,
                " de synoniemen '", .data$Soorten,
                "' beschouwd als eenzelfde taxon met aggregatie van de bedekkingen (rekening houdend met gedeeltelijke overlap)", #nolint
                collapse = NULL
              )
          ) %>%
          summarise(
            Tekst = paste(.data$TekstOpname, collapse = "; ")
          )
        warning(Tekst$Tekst)
      }
    }

    Dubbels <- Kenmerken %>%
      filter(.data$TypeKenmerk == "studiegroep") %>%
      group_by(.data$ID, .data$Kenmerk) %>%
      summarise(Aantal = n()) %>%
      ungroup() %>%
      filter(.data$Aantal > 1)
    if (nrow(Dubbels) > 0) {
      Tekst <- Dubbels %>%
        group_by(.data$ID) %>%
        summarise(
          Kenmerk = paste(unique(.data$Kenmerk), collapse = ", ")
        ) %>%
        ungroup() %>%
        mutate(
          TekstOpname =
            paste0(
              "Voor opname ", .data$ID, " is het kenmerk '",
              .data$Kenmerk, "' meermaals opgegeven", collapse = NULL
            )
        ) %>%
        summarise(
          Tekst = paste(.data$TekstOpname, collapse = "; ")
        )
      warning(Tekst$Tekst)
    }


    KenmerkenSoort <- KenmerkenSoort %>%
      mutate(
        Kenmerk = .data$NBNTaxonVersionKey,
        NBNTaxonVersionKey = NULL,
        TypeKenmerk = "soort_nbn"
      )

    Kenmerken <- Kenmerken %>%
      filter(
        !tolower(.data$TypeKenmerk) %in% c("soort_latijn", "soort_nl")
      ) %>%
      bind_rows(
        KenmerkenSoort
      ) %>%
      filter(!is.na(.data$Kenmerk)) %>%
      mutate(
        Rijnr = row_number(.data$Kenmerk)
      )

    VegLaagAfwezig <- Kenmerken %>%
      filter(
        tolower(.data$TypeKenmerk) == "soort_nbn",
        is.na(.data$Vegetatielaag)
      )
    if (nrow(VegLaagAfwezig) > 0) {
      warning(
        "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint
      )
    }

    #voor studiegroep de lijstnaam toevoegen
    Kenmerken <- Kenmerken %>%
      mutate(
        Kenmerk =
          ifelse(
            .data$TypeKenmerk == "studiegroep",
            tolower(.data$Kenmerk),
            .data$Kenmerk
          ),
        Kenmerk =
          ifelse(
            .data$Kenmerk == "h2s geur",
            "H2S geur",
            .data$Kenmerk
          )
      )
    StudiegroepKenmerken <- Kenmerken %>%
      filter(.data$TypeKenmerk == "studiegroep")
    controleerInvoerwaarde(
      "Data_soortenKenmerken$Kenmerk",
      StudiegroepKenmerken$Kenmerk,
      "StudieItem", "Waarde", ConnectieLSVIhabitats, Tolower = FALSE
    )
    QueryStudiegroepen <-
      sprintf(
        "SELECT Studiegroep.LijstNaam, StudieItem.Waarde AS StudieItem
        FROM Studiegroep INNER JOIN StudieItem
        ON Studiegroep.Id = StudieItem.StudiegroepId
        WHERE StudieItem.Waarde in ('%s')",
        paste(unique(StudiegroepKenmerken$Kenmerk), collapse = "','")
      )
    Studielijst <-
      dbGetQuery(ConnectieLSVIhabitats, QueryStudiegroepen) %>%
      distinct()
    Kenmerken <- Kenmerken %>%
      left_join(Studielijst, by = c("Kenmerk" = "StudieItem"))

    #Waarde omzetten naar interval (om mee te rekenen)
    VertaaldeKenmerken <-
      vertaalInvoerInterval(
        Kenmerken[
          , c("Rijnr", "Type", "Waarde",
              "Eenheid", "Invoertype")
          ],
        LIJST,
        ConnectieLSVIhabitats
      ) %>%
      rename(
        WaardeMin = .data$Min,
        WaardeMax = .data$Max
      ) %>%
      distinct()

    Kenmerken2 <- Kenmerken %>%
      left_join(
        VertaaldeKenmerken,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL,
        Kenmerk = tolower(.data$Kenmerk)
      )

    return(Kenmerken2)
  }
