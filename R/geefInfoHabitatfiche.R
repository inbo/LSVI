#' @title Geeft tabel met info uit de LSVI-rapporten voor de opgegeven parameters
#'
#' @description Deze functie geeft de inhoud van de tabellen habitatkarakteristieken en beoordelingsmatrix uit de rapporten van de Lokale Staat van Instandhouding voor de habitattypes die voldoen aan de opgegeven parameters.  Volledigheidshalve geeft ze ook de uitgebreide namen van de habitattypes en habitatsubtypes.  De uitvoer van deze functie kan gebruikt worden om rapportages op te maken (bv. rapport samenstellen met LSVI-criteria,...).  Een 'afgewerkt rapport' kan gegenereerd worden met de functie maakHabitatfiches().
#'
#'@template Zoekparameters
#'
#' @inheritParams selecteerIndicatoren
#' @param Stijl Keuze uit "Rmd" en "tekst".  Bij Rmd (default) worden soortgroepnamen voorafgegaan en gevolgd door "__" en Latijnse namen van soorten door "_", waardoor deze bij gebruik van RMarkdown worden omgezet naar resp. vet en italics.  Bij tekst worden deze underscores weggelaten.
#'
#' @return Deze functie genereert een tabel met alle gegevens die nodig zijn om de tabellen habitatkarakteristieken en beoordelingsmatrix uit de LSVI-rapporten te genereren.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar het voorbeeld werkt en kan zeker
#' # uitgetest worden.
#' \dontrun{
#' library(LSVI)
#' maakConnectiePool()
#' geefInfoHabitatfiche(Versie = "Versie 2.0", Habitattype = "4030")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr arrange distinct mutate group_by summarise ungroup select left_join filter mutate_
#' @importFrom rlang .data
#' @importFrom lazyeval interp
#' @importFrom assertthat assert_that
#'

geefInfoHabitatfiche <-
  function(Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           Criterium = "alle",
           Indicator = "alle",
           Stijl = c("Rmd", "tekst"),
           ConnectieLSVIhabitats = NULL){

    match.arg(Stijl)
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

    Selectiegegevens <-
      selecteerIndicatoren(
        Versie = Versie,
        Habitatgroep = Habitatgroep,
        Habitattype = Habitattype,
        Criterium = Criterium,
        Indicator = Indicator,
        HabitatnamenToevoegen = TRUE,
        ConnectieLSVIhabitats
      )

    Indicator_hIDs <-
      paste(
        unique(
          (Selectiegegevens %>%
             filter(!is.na(.data$Indicator_habitatID)))$Indicator_habitatID
        ),
        collapse = "','"
      )

    query_habitatfiche <- sprintf(
      "SELECT Indicator_habitat.Id AS Indicator_habitatID,
      cast(Indicator_habitat.Beschrijving AS nvarchar(1050)) AS Beschrijving,
      cast(Indicator_habitat.Beschrijving_naSoorten AS nvarchar(200))
        AS Beschrijving_naSoorten,
      cast(Indicator_habitat.Maatregelen AS nvarchar(510)) AS Maatregelen,
      cast(Indicator_habitat.Opmerkingen AS nvarchar(830)) AS Opmerkingen,
      cast(Indicator_habitat.Referenties AS nvarchar(260)) AS Referenties,
      Indicator_habitat.TaxongroepId
      FROM Indicator_habitat
      WHERE Indicator_habitat.Id in ('%s')",
      Indicator_hIDs
    )

    Indicator_bIDs <-
      paste(
        unique(
          (Selectiegegevens %>%
             filter(
               !is.na(.data$Indicator_beoordelingID)
             )
           )$Indicator_beoordelingID
        ),
        collapse = "','"
      )

    query_beoordelingsfiche <- sprintf(
      "SELECT Indicator_beoordeling.Id AS Indicator_beoordelingID,
      Criterium.Naam AS Criterium, Indicator.Naam As Indicator,
      cast(Indicator_beoordeling.Opmerkingen AS nvarchar(710)) AS Opmerkingen,
      cast(Indicator_beoordeling.Referenties AS nvarchar(150)) AS Referenties,
      Beoordeling.Kwaliteitsniveau,
      cast(Beoordeling.Beoordeling_letterlijk AS nvarchar(360))
        AS Beoordeling_letterlijk
      FROM
      (Indicator_beoordeling INNER JOIN Beoordeling
        ON Indicator_beoordeling.Id = Beoordeling.Indicator_beoordelingID)
      INNER JOIN
        (Indicator INNER JOIN Criterium on Indicator.CriteriumID = Criterium.Id)
      ON Indicator_beoordeling.IndicatorID = Indicator.Id
      WHERE Indicator_beoordeling.Id in ('%s')",
      Indicator_bIDs
    )

    Habitatkarakteristieken <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        query_habitatfiche
      )

    Beoordelingsmatrix <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        query_beoordelingsfiche
      )

    paste2 <- function(..., sep=", ") {
      L <- list(...)
      L <-
        lapply(
          L,
          function(x) {
            x[is.na(x)] <- ""; x
          }
        )
      gsub(paste0("(^", sep, "|", sep, "$)"), "",
           gsub(paste0(sep, sep), sep,
                do.call(paste, c(L, list(sep = sep)))))
    }

    if (!all(is.na(Habitatkarakteristieken$TaxongroepId))) {
      Soortenlijst <-
        geefSoortenlijst(
          Versie = Versie,
          Habitatgroep = Habitatgroep,
          Habitattype = Habitattype,
          Criterium = Criterium,
          Indicator = Indicator,
          Taxonlijstniveau = "indicator",
          Taxonlijsttype = "LSVIfiche",
          ConnectieLSVIhabitats = ConnectieLSVIhabitats
        ) %>%
        filter(!is.na(.data$WetNaamKort) | !is.na(.data$NedNaam)) %>%
        mutate(
          Versie = NULL,
          Habitattype = NULL,
          Habitatsubtype = NULL,
          Indicator_habitatID = NULL,
          Indicator_beoordelingID = NULL,
          TotNaam =
            ifelse(
              is.na(.data$WetNaamKort),
              .data$NedNaam,
              ifelse(
                is.na(.data$NedNaam),
                sprintf("_%s_", .data$WetNaamKort),
                sprintf("%s (_%s_)", .data$NedNaam, .data$WetNaamKort)
              )
            )
        ) %>%
        distinct() %>%
        arrange(.data$TotNaam)

      OmschrijvingKolommen <- NULL
      for (i in colnames(Soortenlijst)) {
        if (grepl("Omschrijving", i)) {
          OmschrijvingKolommen <- c(OmschrijvingKolommen, i)
        }
      }
      OmschrijvingKolommen <-
        OmschrijvingKolommen[
          order(
            as.integer(
              substr(OmschrijvingKolommen, 13, nchar(OmschrijvingKolommen))
            ),
            decreasing = TRUE
          )
        ]

      Soortenlijst <- Soortenlijst %>%
        group_by(
          .data$TaxongroepId,
          .data$Criterium,
          .data$Indicator,
          .dots = OmschrijvingKolommen
        ) %>%
        summarise(
          Soortenlijst = paste(as.vector(.data$TotNaam), collapse = ", ")
        ) %>%
        ungroup()

      laatste_i <- 0
      for (i in seq_len(length(OmschrijvingKolommen))) {
        laatste_i <- max(laatste_i, length(OmschrijvingKolommen))
        Soortenlijst <- Soortenlijst %>%
          mutate_(
            Soortenlijst =
              interp(
                ~ ifelse(
                  is.na(var),
                  Soortenlijst,
                  paste("__", var, ":__ ", Soortenlijst, sep = "")
                ),
                var = as.name(OmschrijvingKolommen[1])
              )
          ) %>%
          select(
            -dplyr::matches(OmschrijvingKolommen[1])
          )

        OmschrijvingKolommen <- OmschrijvingKolommen[-1]

        if (i < laatste_i) {
          Soortenlijst <- Soortenlijst %>%
            group_by(
              .data$TaxongroepId,
              .data$Criterium,
              .data$Indicator,
              .dots = OmschrijvingKolommen
            ) %>%
            summarise(
              Soortenlijst =
                paste(as.vector(.data$Soortenlijst), collapse = ",  ")
            ) %>%
            ungroup()
        } else {
          Soortenlijst <- Soortenlijst %>%
            group_by(
              .data$TaxongroepId,
              .data$Criterium,
              .data$Indicator
            ) %>%
            summarise(
              Soortenlijst =
                paste(as.vector(.data$Soortenlijst), collapse = ",  ")
            ) %>%
            ungroup()
        }

      }

      Habitatfiche <- Selectiegegevens %>%
        left_join(
          Habitatkarakteristieken %>%
            mutate(TaxongroepId = NULL),
          by = c("Indicator_habitatID" = "Indicator_habitatID")
        ) %>%
        left_join(
          Soortenlijst %>%
            select(
              .data$TaxongroepId,
              .data$Soortenlijst
              ),
          by = c("TaxongroepId" = "TaxongroepId")
        ) %>%
        mutate(
          Beschrijving =
            paste2(
              .data$Beschrijving,
              .data$Soortenlijst,
              .data$Beschrijving_naSoorten,
              sep = " "
            )
        ) %>%
        left_join(
          Beoordelingsmatrix,
          by = c("Indicator_beoordelingID" = "Indicator_beoordelingID"),
          suffix = c(".habitat", ".beoordeling")
        ) %>%
        select(
          .data$Versie, .data$Habitattype, .data$Habitatnaam,
          .data$Habitatsubtype,
          .data$Habitatsubtypenaam, .data$HabitatsubtypeOmschrijving,
          .data$Criterium.habitat,
          .data$Indicator.habitat, .data$Beschrijving, .data$Maatregelen,
          .data$Opmerkingen.habitat, .data$Referenties.habitat,
          .data$Soortenlijst,
          Beoordeling = .data$Beoordeling_letterlijk,
          .data$Criterium.beoordeling,
          .data$Indicator.beoordeling, .data$Opmerkingen.beoordeling,
          .data$Referenties.beoordeling, .data$Kwaliteitsniveau
        )
    } else {

      Habitatfiche <- Selectiegegevens %>%
        left_join(
          Habitatkarakteristieken,
          by = c("Indicator_habitatID" = "Indicator_habitatID")
        ) %>%
        mutate(
          Beschrijving =
            paste2(.data$Beschrijving, .data$Beschrijving_naSoorten, sep = " ")
        ) %>%
        left_join(
          Beoordelingsmatrix,
          by = c("Indicator_beoordelingID" = "Indicator_beoordelingID"),
          suffix = c(".habitat", ".beoordeling")
        ) %>%
        mutate(Soortenlijst = NA) %>%
        select(
          .data$Versie, .data$Habitattype, .data$Habitatnaam,
          .data$Habitatsubtype,
          .data$Habitatsubtypenaam, .data$HabitatsubtypeOmschrijving,
          .data$Criterium.habitat,
          .data$Indicator.habitat, .data$Beschrijving, .data$Maatregelen,
          .data$Opmerkingen.habitat, .data$Referenties.habitat,
          .data$Soortenlijst,
          Beoordeling = .data$Beoordeling_letterlijk,
          .data$Criterium.beoordeling,
          .data$Indicator.beoordeling, .data$Opmerkingen.beoordeling,
          .data$Referenties.beoordeling, .data$Kwaliteitsniveau
        )
    }


    if (Stijl[1] == "tekst") {
      Habitatfiche$Habitatnaam <- gsub("_", "", Habitatfiche$Habitatnaam)
      Habitatfiche$Habitatsubtypenaam <-
        gsub("_", "", Habitatfiche$Habitatsubtypenaam)
      Habitatfiche$HabitatsubtypeOmschrijving <-
        gsub("_", "", Habitatfiche$HabitatsubtypeOmschrijving)
      Habitatfiche$Beschrijving <- gsub("_", "", Habitatfiche$Beschrijving)
      Habitatfiche$Maatregelen <-
        gsub("_", "", Habitatfiche$Maatregelen)
      Habitatfiche$Opmerkingen.habitat <-
        gsub("_", "", Habitatfiche$Opmerkingen.habitat)
      Habitatfiche$Soortenlijst <- gsub("_", "", Habitatfiche$Soortenlijst)
      Habitatfiche$Opmerkingen.beoordeling <-
        gsub("_", "", Habitatfiche$Opmerkingen.beoordeling)
      Habitatfiche$Beoordeling_letterlijk <-
        gsub("_", "", Habitatfiche$Beoordeling)
    }

    return(Habitatfiche)

  }
