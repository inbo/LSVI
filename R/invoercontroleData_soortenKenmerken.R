#' Invoercontrole voor dataframe Data_soortenKenmerken
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).  Ingeval van Data_soortenKenmerken is ook de omzetting van soortnamen naar een NBNTaxonVersionKey en de omzettingen van bedekkingen naar een interval opgenomen in de functie.
#'
#' @param Data_soortenKenmerken dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% filter mutate select left_join bind_rows rename
#' @importFrom rlang .data
#' @importFrom rgbif parsenames
#'
#' @export
#'
invoercontroleData_soortenKenmerken <-
  function(Data_soortenKenmerken, ConnectieLSVIhabitats, LIJST) {
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )

    assert_that(inherits(Data_soortenKenmerken, "data.frame"))
    assert_that(has_name(Data_soortenKenmerken, "ID"))
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
    assert_that(
      all(
        tolower(Data_soortenKenmerken$TypeKenmerk) %in%
          c("studiegroep", "soort_nbn", "soort_latijn", "soort_nl", "doodhout")
      ),
      msg = "TypeKenmerk moet een van de volgende waarden zijn: studiegroep, soort_nbn, soort_latijn, soort_nl" #nolint
    )
    assert_that(has_name(Data_soortenKenmerken, "Waarde"))
    assert_that(has_name(Data_soortenKenmerken, "Type"))
    if (!is.character(Data_soortenKenmerken$Type)) {
      Data_soortenKenmerken$Type <-
        as.character(Data_soortenKenmerken$Type)
    }
    if (
      !all(Data_soortenKenmerken$Type %in%
           geefUniekeWaarden(
             "TypeVariabele",
             "Naam",
             ConnectieLSVIhabitats
           )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Type komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_soortenKenmerken, "Invoertype"))
    if (!is.character(Data_soortenKenmerken$Invoertype)) {
      Data_soortenKenmerken$Invoertype <-
        as.character(Data_soortenKenmerken$Invoertype)
    }
    if (!all(is.na(Data_soortenKenmerken$Invoertype) |
             Data_soortenKenmerken$Invoertype %in%
             geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
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
        "Aantal_ha"
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
    GeldigeWaarden <-
      c(
        geefUniekeWaarden(
          "StudieItem",
          "Waarde",
          ConnectieLSVIhabitats
        )
      )
    if (
      !all(
        Data_soortenKenmerken$Vegetatielaag %in% GeldigeWaarden
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Vegetatielaag komen overeen met waarden vermeld in de databank.") #nolint
    }



    #○mzettingen naar een bruikbare dataframe
    Kenmerken <- Data_soortenKenmerken #naamsverandering is omdat code verplaatst is

    QuerySoorten <-
      "SELECT TaxonSynoniem.FloraNaamNederlands AS NedNaam,
          TaxonSynoniem.NbnNaam AS WetNaamKort,
          Taxon.NBNTaxonVersionKey, Taxon.TaxonTypeId
      FROM TaxonSynoniem INNER JOIN Taxon
        ON TaxonSynoniem.TaxonId = Taxon.Id
      WHERE Taxon.NBNTaxonVersionKey IS NOT NULL"

    Taxonlijst <-
      dbGetQuery(ConnectieLSVIhabitats, QuerySoorten)

    #onderstaande code mag weg zodra Gert deze Canonicalname toegevoegd heeft aan de databank
    Taxonlijst <- Taxonlijst %>%
      mutate(
        Canonicalname = parsenames(.data$WetNaamKort)$canonicalnamewithmarker
      )

    KenmerkenSoort <- Kenmerken %>%
      filter(tolower(.data$TypeKenmerk) == "soort_latijn") %>%
      mutate(
        Canonicalname =
          parsenames(.data$Kenmerk)$canonicalnamewithmarker
      ) %>%
      left_join(
        Taxonlijst %>%
          select(
            .data$Canonicalname, .data$NBNTaxonVersionKey
          ),
        by = c("Canonicalname")
      ) %>%
      bind_rows(
        Kenmerken %>%
          filter(tolower(.data$TypeKenmerk) == "soort_nl") %>%
          left_join(
            Taxonlijst %>%
              select(
                .data$NedNaam, .data$NBNTaxonVersionKey
              ),
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
          paste(unique(Fouten$Kenmerk))
        )
      )
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
      )

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
