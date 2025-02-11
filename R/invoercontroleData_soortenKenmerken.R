#' Invoercontrole voor dataframe Data_soortenKenmerken
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en
#' om de hoofdscripts overzichtelijk te houden, maken we voor elke
#' invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze
#' wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund
#' worden (maar worden wel gerund als de functie waarin ze voorkomen,
#' aangeroepen wordt).  Ingeval van Data_soortenKenmerken is ook het koppelen
#' van soortnamen aan GbifUsageKeys van verschillende niveaus
#' (van Kingdom tot Subspecies, Form of Variety)
#' en de omzettingen van bedekkingen naar een interval opgenomen in de functie.
#'
#' @param Data_soortenKenmerken dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% bind_rows distinct filter left_join mutate n rename
#'   select
#' @importFrom purrr map map_dbl
#' @importFrom readr read_csv2
#' @importFrom rgbif name_backbone_checklist name_usage
#' @importFrom rlang .data sym
#' @importFrom stringr str_to_sentence
#' @importFrom tidyr unnest_wider
#' @importFrom tools toTitleCase
#'
#' @export
#'
invoercontroleData_soortenKenmerken <- #nolint: object_name_linter
  function(Data_soortenKenmerken, ConnectieLSVIhabitats, LIJST) { #nolint: object_name_linter, line_length_linter
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )

    assert_that(inherits(Data_soortenKenmerken, "data.frame"))
    assert_that(has_name(Data_soortenKenmerken, "ID"))
    if (!is.character(Data_soortenKenmerken$ID)) {
      Data_soortenKenmerken$ID <- as.character(Data_soortenKenmerken$ID) #nolint: object_name_linter, line_length_linter
    }
    assert_that(has_name(Data_soortenKenmerken, "Kenmerk"))
    if (!is.character(Data_soortenKenmerken$Kenmerk)) {
      Data_soortenKenmerken$Kenmerk <- #nolint: object_name_linter
        as.character(Data_soortenKenmerken$Kenmerk)
    }
    assert_that(has_name(Data_soortenKenmerken, "TypeKenmerk"))
    if (!is.character(Data_soortenKenmerken$TypeKenmerk)) {
      Data_soortenKenmerken$TypeKenmerk <- #nolint: object_name_linter
        as.character(Data_soortenKenmerken$TypeKenmerk)
    }
    Data_soortenKenmerken$TypeKenmerk <- #nolint: object_name_linter
      tolower(Data_soortenKenmerken$TypeKenmerk)
    assert_that(
      all(
        Data_soortenKenmerken$TypeKenmerk %in%
          c("studiegroep", "soort_gbif", "soort_latijn", "soort_nl",
            "soort_nbn")
      ),
      msg = "Data_soortenKenmerken$TypeKenmerk moet een van de volgende waarden zijn: studiegroep, soort_gbif, soort_latijn, soort_nl" #nolint: line_length_linter
    )
    if (any(Data_soortenKenmerken$TypeKenmerk == "soort_nbn")) {
      warning("Het gebruik van de NbnTaxonVersionKey (en TypeKenmerk 'soort_nbn') in Data_soortenKenmerken wordt afgebouwd (is deprecated) en zal in de volgende versie van het package LSVI niet meer mogelijk zijn.") #nolint: line_length_linter
    }
    assert_that(has_name(Data_soortenKenmerken, "Waarde"))
    if (!is.character(Data_soortenKenmerken$Waarde)) {
      Data_soortenKenmerken$Waarde <- #nolint: object_name_linter
        as.character(Data_soortenKenmerken$Waarde)
    }
    assert_that(has_name(Data_soortenKenmerken, "Type"))
    if (!is.character(Data_soortenKenmerken$Type)) {
      Data_soortenKenmerken$Type <- #nolint: object_name_linter
        as.character(Data_soortenKenmerken$Type)
    }
    Data_soortenKenmerken$Type <- str_to_sentence(Data_soortenKenmerken$Type) #nolint: object_name_linter, line_length_linter
    controleerInvoerwaarde(
      "Data_soortenKenmerken$Type", Data_soortenKenmerken$Type,
      "TypeVariabele", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )
    assert_that(has_name(Data_soortenKenmerken, "Invoertype"))
    if (!is.character(Data_soortenKenmerken$Invoertype)) {
      Data_soortenKenmerken$Invoertype <- #nolint: object_name_linter
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
      Data_soortenKenmerken$Eenheid <- #nolint: object_name_linter
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
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Eenheid komen overeen met waarden vermeld in de databank.") #nolint: line_length_linter
    }

    assert_that(has_name(Data_soortenKenmerken, "Vegetatielaag"))
    if (!is.character(Data_soortenKenmerken$Vegetatielaag)) {
      Data_soortenKenmerken$Vegetatielaag <- #nolint: object_name_linter
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

    Taxonlijst <-
      suppressMessages(
        read_csv2(
          system.file("databank/TaxonTabel.csv", package = "LSVI"),
          show_col_types = FALSE
        )
      ) %>%
      mutate(
        AcceptedKey =
          ifelse(
            is.na(.data$GbifAcceptedUsageKey),
            .data$GbifUsageKey,
            .data$GbifAcceptedUsageKey
          ),
        # voor de verdere code zijn volgende kolommen nodig,
        # maar in de lijst staan enkel exacte matches en die is gecontroleerd op
        # juistheid, dus deze waarden meegeven aan gebruiker is niet nodig
        # (en met deze code is het ook niet nodig om ze in de lijst te hebben)
        GbifMatchType = NA_character_,
        GbifConfidence = NA_integer_
      )


    # Soorten rechtstreeks proberen te koppelen met taxonlijst uit package
    KenmerkenSoort <- Kenmerken %>%
      filter(tolower(.data$TypeKenmerk) == "soort_latijn") %>%
      left_join(
        Taxonlijst %>%
          select(
            -"ScientificNameExact", -"NLNameExact", -"NbnTaxonVersionKey"
          ) %>%
          distinct(),
        by = c("Kenmerk" = "TaxonNameExact")
      ) %>%
      bind_rows(
        Kenmerken %>%
          filter(tolower(.data$TypeKenmerk) == "soort_nl") %>%
          mutate(
            Kenmerk = tolower(.data$Kenmerk)
          ) %>%
          left_join(
            Taxonlijst %>%
              select(
                -"ScientificNameExact", -"TaxonNameExact", -"NbnTaxonVersionKey"
              ) %>%
              filter(!is.na(.data$NLNameExact)) %>%
              distinct() %>%
              mutate(
                NLNameExact = tolower(.data$NLNameExact)
              ),
            by = c("Kenmerk" = "NLNameExact")
          )
      ) %>%
      bind_rows(
        Kenmerken %>%
          filter(tolower(.data$TypeKenmerk) == "soort_gbif") %>%
          left_join(
            Taxonlijst %>%
              mutate(
                Kenmerk = as.character(.data$GbifUsageKey)
              ) %>%
              select(
                -"ScientificNameExact", -"TaxonNameExact", -"NLNameExact",
                -"NbnTaxonVersionKey"
              ) %>%
              distinct(),
            by = c("Kenmerk")
          )
      ) %>%
      bind_rows(
        Kenmerken %>%
          filter(tolower(.data$TypeKenmerk) == "soort_nbn") %>%
          left_join(
            Taxonlijst %>%
              mutate(
                Kenmerk = as.character(.data$NbnTaxonVersionKey)
              ) %>%
              select(
                -"ScientificNameExact", -"TaxonNameExact", -"NLNameExact"
              ) %>%
              distinct(),
            by = c("Kenmerk")
          )
      ) %>%
      mutate(
        Koppelmethode = "exacte naam/key in LSVI-package"
      )

    # Latijnse namen opzoeken via rgbif
    GbifLatijn <- KenmerkenSoort %>%
      filter(
        is.na(.data$AcceptedKey),
        tolower(.data$TypeKenmerk) == "soort_latijn"
      ) %>%
      distinct(.data$Kenmerk)
    if (nrow(GbifLatijn) > 0) {
      GbifLatijn <- GbifLatijn %>%
        mutate(
          name_backbone_checklist(.data$Kenmerk)
        ) %>%
        transmute(
          .data$Kenmerk,
          GbifUsageKey = .data$usageKey,
          GbifConfidence = .data$confidence,
          GbifMatchType = .data$matchType,
          GbifAcceptedUsageKey =
            ifelse(!.data$synonym, NA, .data$acceptedUsageKey)
        ) %>%
        # Met info uit package proberen te koppelen via usagekey
        left_join(
          Taxonlijst %>%
            select(
              -"TaxonNameExact", -"ScientificNameExact", -"GbifConfidence",
              -"GbifMatchType", -"NLNameExact", -"NbnTaxonVersionKey"
            ) %>%
            distinct(),
          by = "GbifUsageKey",
          suffix = c("MagWeg", "")
        )
      Onbetrouwbaar <- GbifLatijn %>%
        filter(.data$GbifMatchType != "EXACT")
      if (nrow(Onbetrouwbaar) > 0) {
        stop(
          paste0(
            "Latijnse naam/namen ",
            paste(unique(Onbetrouwbaar$Kenmerk), collapse = ", "),
            " geeft/geven geen betrouwbaar resultaat bij het opzoeken van de",
            " Gbif-key met functie rgbif::name_backbone(). Check de spelling",
            " en kijk na of het een Latijnse naam (inclusief auteursnaam) is."
          )
        )
      }
      GbifLatijn <- GbifLatijn %>%
        filter(!is.na(.data$AcceptedKey)) %>%
        mutate(
          Koppelmethode = "Gbif-usagekey opgezocht voor Latijnse naam",
          GbifAcceptedUsageKeyMagWeg = NULL
        ) %>%
        bind_rows(
          GbifLatijn %>%
            filter(is.na(.data$AcceptedKey)) %>%
            select(
              "Kenmerk", "GbifUsageKey", "GbifConfidence", "GbifMatchType",
              "GbifAcceptedUsageKey"
            ) %>%
            left_join(
              Taxonlijst %>%
                select(
                  -"TaxonNameExact", -"ScientificNameExact", -"GbifConfidence",
                  -"GbifMatchType", -"NLNameExact", -"NbnTaxonVersionKey",
                  -"GbifAcceptedUsageKey"
                ) %>%
                distinct(),
              by = c("GbifAcceptedUsageKey" = "GbifUsageKey")
            ) %>%
            mutate(
              Koppelmethode = "Gbif-acceptedkey opgezocht voor Latijnse naam"
            )
        )
    }

    # Nederlandse namen opzoeken via rgbif
    GbifNL <- KenmerkenSoort %>%
      filter(
        is.na(.data$AcceptedKey),
        tolower(.data$TypeKenmerk) == "soort_nl"
      ) %>%
      distinct(.data$Kenmerk)
    if (nrow(GbifNL) > 0) {
      # extra record toevoegen omdat map_taxa_from_vernacular() vastloopt
      # als eerste record onbekende soort is
      GbifNL <- data.frame(
        Kenmerk = "Eekhoorn", Kingdom = "Animalia", Class = "Mammalia"
      ) %>%
        bind_rows(
          merge(
            GbifNL,
            data.frame(Kingdom = c("Plantae", "Fungi"))
          )
        )
      GbifNL <- map_taxa_from_vernacular(
        vernacular_name_df = GbifNL,
        vernacular_name_col = "Kenmerk",
        out_cols = c("scientificName", "nubKey", "synonym", "acceptedKey"),
        filter_cols = list(kingdom = "Kingdom"),
        lang = "nld",
        limit = 1000,
        increment = 250
      ) %>%
        filter(!is.na(.data$scientificName), .data$Kenmerk != "Eekhoorn")
      if (all(c("synonym", "acceptedKey") %in% colnames(GbifNL))) {
        GbifNL <- GbifNL %>%
          mutate(
            GbifAcceptedUsageKey =
              ifelse(
                is.na(.data$synonym) | !.data$synonym,
                NA, .data$acceptedUsageKey
              )
          )
      } else {
        GbifNL$GbifAcceptedUsageKey <- NA_integer_
      }
      GbifNL <- GbifNL %>%
        transmute(
          .data$Kenmerk,
          GbifUsageKey = .data$nubKey,
          .data$GbifAcceptedUsageKey
        ) %>%
        # Met info uit package proberen te koppelen via usagekey
        left_join(
          Taxonlijst %>%
            select(
              -"TaxonNameExact", -"ScientificNameExact", -"GbifConfidence",
              -"GbifMatchType", -"NLNameExact", -"NbnTaxonVersionKey"
            ) %>%
            distinct(),
          by = "GbifUsageKey",
          suffix = c("MagWeg", "")
        )
      GbifNL <- GbifNL %>%
        filter(!is.na(.data$AcceptedKey)) %>%
        mutate(
          Koppelmethode = "Gbif-usagekey opgezocht voor Nederlandse naam",
          GbifAcceptedUsageKeyMagWeg = NULL
        ) %>%
        bind_rows(
          GbifNL %>%
            filter(is.na(.data$AcceptedKey)) %>%
            select(
              "Kenmerk", "GbifUsageKey", "GbifAcceptedUsageKey"
            ) %>%
            left_join(
              Taxonlijst %>%
                select(
                  -"TaxonNameExact", -"ScientificNameExact", -"GbifConfidence",
                  -"GbifMatchType", -"NLNameExact", -"NbnTaxonVersionKey",
                  -"GbifAcceptedUsageKey"
                ) %>%
                distinct(),
              by = c("GbifAcceptedUsageKey" = "GbifUsageKey")
            ) %>%
            mutate(
              Koppelmethode =
                "Gbif-acceptedkey opgezocht voor Nederlandse naam"
            )
        )
    }
    geefGbifKey <- function(Key) {
      if (grepl("\\D", Key)) {
        stop(
          "Geef bij een Kenmerk met TypeKenmerk soort_gbif enkel cijfers, geen letters of andere tekens" #nolint: line_length_linter
        )
      }
      GbifUitvoer <- name_usage(Key)$data
      if (has_name(GbifUitvoer, "acceptedKey")) {
        return(GbifUitvoer$acceptedKey)
      } else {
        return(NA)
      }
    }

    KenmerkenSoort <- KenmerkenSoort %>%
      filter(
        !is.na(.data$AcceptedKey) |
          (is.na(.data$AcceptedKey) & tolower(.data$TypeKenmerk) == "soort_nbn")
      ) %>%
      bind_rows(
        KenmerkenSoort %>%
          filter(
            is.na(.data$AcceptedKey),
            tolower(.data$TypeKenmerk) == "soort_latijn"
          ) %>%
          select(
            "ID", "Kenmerk", "TypeKenmerk", "Waarde", "Type", "Invoertype",
            "Eenheid", "Vegetatielaag"
          ) %>%
          left_join(
            GbifLatijn,
            by = "Kenmerk"
          )
      ) %>%
      bind_rows(
        KenmerkenSoort %>%
          filter(
            is.na(.data$AcceptedKey),
            tolower(.data$TypeKenmerk) == "soort_nl"
          ) %>%
          select(
            "ID", "Kenmerk", "TypeKenmerk", "Waarde", "Type", "Invoertype",
            "Eenheid", "Vegetatielaag"
          ) %>%
          left_join(
            GbifNL,
            by = "Kenmerk"
          )
      ) %>%
      bind_rows(
        KenmerkenSoort %>%
          filter(
            is.na(.data$AcceptedKey),
            tolower(.data$TypeKenmerk) == "soort_gbif"
          ) %>%
          select(
            "ID", "Kenmerk", "TypeKenmerk", "Waarde", "Type", "Invoertype",
            "Eenheid", "Vegetatielaag"
          ) %>%
          mutate(
            AcceptedKey = map_dbl(.data$Kenmerk, geefGbifKey)
          ) %>%
          left_join(
            Taxonlijst %>%
              select(
                -"TaxonNameExact", -"ScientificNameExact",
                -"GbifConfidence", -"GbifMatchType", -"NLNameExact",
                -"AcceptedKey", -"NbnTaxonVersionKey"
              ) %>%
              distinct() %>%
              mutate(
                Koppelmethode = "Gbif-acceptedkey opgezocht voor gbif-key"
              ),
            by = c("AcceptedKey" = "GbifUsageKey")
          )
      )

    # Foutmelding als geen accepted key gevonden, m.a.w. geen record in de lijst
    # en kan niet gekoppeld worden in gbif
    Fouten <- KenmerkenSoort %>%
      filter(is.na(.data$AcceptedKey))
    if (nrow(Fouten) > 0) {
      stop(
        sprintf(
          "Volgende soortnamen of keys zijn niet teruggevonden in de databank of Gbif: %s.  Check de spelling en/of ga na of de gebruikte taxonomie nog gangbaar is.",  #nolint: line_length_linter
          paste(unique(Fouten$Kenmerk), collapse = ", ")
        )
      )
    }

    # Als wel accepted key maar geen andere info, is de info niet aanwezig
    # in de lijst en moeten we op Gbif voortgaan
    geefInfoGbif <- function(Key) {
      name_usage(Key)$data %>%
        transmute(
          Rank = .data$rank,
          Kingdom = .data$kingdom,
          Phylum = .data$phylum,
          Order = .data$order,
          Family = .data$family,
          Genus = .data$genus,
          Species = .data$species,
          KingdomKey = .data$kingdomKey,
          PhylumKey = .data$phylumKey,
          ClassKey = .data$classKey,
          OrderKey = .data$orderKey,
          FamilyKey = .data$familyKey,
          GenusKey = .data$genusKey,
          SpeciesKey = .data$speciesKey,
          Koppelmethode = "volledige taxoninfo uit Gbif"
        )
    }
    KenmerkenSoort <- KenmerkenSoort %>%
      filter(!is.na(.data$Rank)) %>%
      bind_rows(
        KenmerkenSoort %>%
          filter(is.na(.data$Rank)) %>%
          transmute(
            .data$ID, .data$Kenmerk, .data$TypeKenmerk, .data$Waarde,
            .data$Type, .data$Invoertype, .data$Eenheid, .data$Vegetatielaag,
            .data$GbifUsageKey, .data$GbifConfidence, .data$GbifMatchType,
            .data$GbifAcceptedUsageKey, .data$AcceptedKey,
            InfoGbif = map(.data$AcceptedKey, geefInfoGbif)
          ) %>%
          unnest_wider("InfoGbif")
      )


    Fouten <- KenmerkenSoort %>%
      filter(is.na(.data$Rank))

    if (nrow(Fouten) > 0) {
      warning(
        sprintf(
          "Er is iets vreemds aan de hand: aan de volgend soorten kan geen rank toegekend worden terwijl ze wel gekend zijn in Gbif: %s.  Check de juistheid van de ingevoerde soorten en geef dit probleem door aan de beheerder van het package LSVI als dit zich blijft voordoen.",  #nolint: line_length_linter
          paste(unique(Fouten$Kenmerk), collapse = ", ")
        )
      )
    }

    Dubbels <- KenmerkenSoort %>%
      group_by(
        .data$ID, .data$AcceptedKey, .data$Rank, .data$Vegetatielaag,
        .data$Eenheid, .data$Kenmerk
      ) %>%
      summarise(Aantal = n()) %>%
      ungroup() %>%
      filter(.data$Aantal > 1)
    if (nrow(Dubbels) > 0) {
      Tekst <- Dubbels %>%
        inner_join(
          KenmerkenSoort,
          by = c("ID", "AcceptedKey", "Rank", "Vegetatielaag", "Eenheid",
                 "Kenmerk")
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
      stop(Tekst$Tekst)
    }

    Synoniemen <- KenmerkenSoort %>%
      group_by(
        .data$ID, .data$AcceptedKey, .data$Rank, .data$Vegetatielaag,
        .data$Eenheid
      ) %>%
      summarise(Aantal = n()) %>%
      ungroup() %>%
      filter(.data$Aantal > 1)
    if (nrow(Synoniemen) > 0) {
      Synoniemen <- Synoniemen %>%
        inner_join(
          KenmerkenSoort,
          by = c("ID", "AcceptedKey", "Rank", "Vegetatielaag", "Eenheid")
        )
      LatijnEnNl <- Synoniemen %>%
        group_by(
          .data$ID, .data$AcceptedKey, .data$Rank, .data$Vegetatielaag,
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
                " meerdere namen / keys gebruikt voor de soort '",
                .data$Soorten, "'", collapse = NULL
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
                "' beschouwd als eenzelfde taxon met aggregatie van de bedekkingen (rekening houdend met gedeeltelijke overlap)", #nolint: line_length_linter
                collapse = NULL
              )
          ) %>%
          summarise(
            Tekst = paste(.data$TekstOpname, collapse = "; ")
          )
        warning(Tekst$Tekst)
      }
    }

    # Warning als (overlappende) taxa van verschillende niveaus opgegeven zijn
    Niveaus <- c("SUBSPECIES", "VARIETY", "FORM")
    for (Niveau in c("SPECIES", "GENUS", "FAMILY", "ORDER", "CLASS", "PHYLUM")
         ) {
      Niveaus <- c(Niveaus, Niveau)
      Kolomnaam <- paste0(toTitleCase(tolower(Niveau)), "Key")
      Synoniemen <- KenmerkenSoort %>%
        filter(.data$Rank %in% Niveaus) %>%
        group_by(
          .data$ID, !!sym(Kolomnaam), .data$Vegetatielaag, .data$Eenheid
        ) %>%
        summarise(
          Aantal = n(),
          AantalNiveau = sum(.data$Rank == Niveau)
        ) %>%
        ungroup() %>%
        filter(
          .data$Aantal > 1,
          .data$AantalNiveau > 0
        )
      if (nrow(Synoniemen) > 0) {
        Synoniemen <- Synoniemen %>%
          inner_join(
            KenmerkenSoort,
            by = c("ID", Kolomnaam, "Vegetatielaag", "Eenheid")
          )
        Tekst <- Synoniemen %>%
          group_by(
            .data$ID, !!sym(Kolomnaam), .data$Vegetatielaag, .data$Eenheid
          ) %>%
          distinct() %>%
          summarise(
            Soorten =
              paste(.data$Rank, .data$Kenmerk, sep = " ", collapse = "' en '"),
          ) %>%
          ungroup() %>%
          mutate(
            TekstOpname =
              paste0(
                "Voor opname ", .data$ID, " zijn in de ", .data$Vegetatielaag,
                " '", .data$Soorten, "' op ", tolower(Niveau),
                "niveau of hoger beschouwd als eenzelfde taxon met aggregatie van de bedekkingen (rekening houdend met gedeeltelijke overlap)", #nolint: line_length_linter
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
      stop(Tekst$Tekst)
    }

    if ("SUBSPECIES" %in% unique(KenmerkenSoort$Rank)) {
      KenmerkenSoort <- KenmerkenSoort %>%
        mutate(
          SubspeciesKey =
            ifelse(.data$Rank == "SUBSPECIES", .data$GbifUsageKey, NA)
        )
    }
    if ("VARIETY" %in% unique(KenmerkenSoort$Rank)) {
      KenmerkenSoort <- KenmerkenSoort %>%
        mutate(
          VarietyKey = ifelse(.data$Rank == "VARIETY", .data$GbifUsageKey, NA)
        )
    }
    if ("FORM" %in% unique(KenmerkenSoort$Rank)) {
      KenmerkenSoort <- KenmerkenSoort %>%
        mutate(
          FormKey = ifelse(.data$Rank == "FORM", .data$GbifUsageKey, NA)
        )
    }

    KenmerkenSoort <- KenmerkenSoort %>%
      mutate(
        TypeKenmerk = "soort_gbif"
      )

    Kenmerken <- Kenmerken %>%
      filter(
        !tolower(.data$TypeKenmerk) %in%
          c("soort_latijn", "soort_nl", "soort_gbif", "soort_nbn")
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
        tolower(.data$TypeKenmerk) == "soort_gbif",
        is.na(.data$Vegetatielaag)
      )
    if (nrow(VegLaagAfwezig) > 0) {
      warning(
        "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint: line_length_linter
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
      select(-"NbnTaxonVersionKey") %>%
      left_join(
        VertaaldeKenmerken,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL
      )

    return(Kenmerken2)
  }
