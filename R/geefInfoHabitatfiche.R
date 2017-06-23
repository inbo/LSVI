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
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' geefInfoHabitatfiche(ConnectieLSVIhabitats, Versie = "Versie 3", Habitattype = "4010")
#' library(RODBC)
#' odbcClose(ConnectieLSVIhabitats)
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr arrange_ mutate_ group_by_ summarise_ ungroup select select_ left_join
#' @importFrom lazyeval interp
#' @importFrom assertthat assert_that
#'

geefInfoHabitatfiche <-
  function(ConnectieLSVIhabitats,
           Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           Criterium = "alle",
           Indicator = "alle",
           Stijl = c("Rmd", "tekst")){

    match.arg(Stijl)
    assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))

    Selectiegegevens <-
      selecteerIndicatoren(ConnectieLSVIhabitats, Versie, Habitatgroep, Habitattype,
                           Criterium, Indicator, HabitatnamenToevoegen = TRUE)

    Indicator_hIDs <-
      paste(unique((Selectiegegevens %>% filter_(~!is.na(Indicator_habitatID)))$Indicator_habitatID),
            collapse = ",")

    query_habitatfiche <- sprintf(
      "SELECT Indicator_habitat.Id AS Indicator_habitatID,
      Indicator_habitat.Beschrijving, Indicator_habitat.Beschrijving_naSoorten,
      Indicator_habitat.Maatregelen, Indicator_habitat.Opmerkingen,
      Indicator_habitat.Referenties, Indicator_habitat.SoortengroepID
      FROM Indicator_habitat
      WHERE Indicator_habitat.Id in (%s)",
      Indicator_hIDs
    )

    Indicator_bIDs <-
      paste(unique((Selectiegegevens %>% filter_(~!is.na(Indicator_beoordelingID)))$Indicator_beoordelingID),
            collapse = ",")

    query_beoordelingsfiche <- sprintf(
      "SELECT Indicator_beoordeling.Id AS Indicator_beoordelingID,
      Criterium.Naam AS Criterium, Indicator.Naam As Indicator,
      Indicator_beoordeling.Opmerkingen, Indicator_beoordeling.Referenties,
      Beoordeling.Kwaliteitsniveau, Beoordeling.Beoordeling_letterlijk
      FROM (Indicator_beoordeling INNER JOIN Beoordeling ON Indicator_beoordeling.Id = Beoordeling.Indicator_beoordelingID)
      INNER JOIN (Indicator INNER JOIN Criterium on Indicator.CriteriumID = Criterium.Id)
      ON Indicator_beoordeling.IndicatorID = Indicator.Id
      WHERE Indicator_beoordeling.Id in (%s)",
      Indicator_bIDs
    )

    Habitatkarakteristieken <- sqlQuery(ConnectieLSVIhabitats, query_habitatfiche, stringsAsFactors = FALSE)

    Beoordelingsmatrix <- sqlQuery(ConnectieLSVIhabitats, query_beoordelingsfiche, stringsAsFactors = FALSE)

    if (!all(is.na(Habitatkarakteristieken$SoortengroepID))) {
      Soortenlijst <-
        geefSoortenlijst(ConnectieLSVIhabitats, Versie, Habitatgroep, Habitattype,
                         Criterium, Indicator, "LSVIfiche") %>%
        filter_(~!is.na(WetNaamKort) | !is.na(NedNaam)) %>%
        mutate_(
          Versie = ~NULL,
          Habitattype = ~NULL,
          Habitatsubtype = ~NULL,
          TotNaam = ~ ifelse(is.na(WetNaamKort),
                             NedNaam,
                             ifelse(is.na(NedNaam),
                                    sprintf("_%s_",WetNaamKort),
                                    sprintf("%s (_%s_)", NedNaam, WetNaamKort)))

        ) %>%
        arrange_(~ TotNaam)

      OmschrijvingKolommen <- NULL
      for (i in colnames(Soortenlijst)) {
        if (grepl("Omschrijving",i)) {
          OmschrijvingKolommen <- c(OmschrijvingKolommen, i)
        }
      }
      OmschrijvingKolommen <-
        OmschrijvingKolommen[order(as.integer(substr(OmschrijvingKolommen, 13, nchar(OmschrijvingKolommen))),
                                   decreasing = TRUE)]

      Soortenlijst <- Soortenlijst %>%
        group_by_(
          ~ SoortengroepID,
          ~ Criterium,
          ~ Indicator,
          .dots = OmschrijvingKolommen
        ) %>%
        summarise_(
          Soortenlijst = ~ paste(as.vector(TotNaam), collapse = ", ")
        ) %>%
        ungroup()

      laatste_i <- 0
      for (i in seq_len(length(OmschrijvingKolommen))) {
        laatste_i <- max(laatste_i, length(OmschrijvingKolommen))
        Soortenlijst <- Soortenlijst %>%
          mutate_(
            Soortenlijst = interp(~ ifelse(is.na(var), Soortenlijst,
                                           paste("__",var,":__ ",Soortenlijst, sep="")),
                                  var = as.name(OmschrijvingKolommen[1]))
          ) %>%
          select(
            -dplyr::matches(OmschrijvingKolommen[1])
          )

        OmschrijvingKolommen <- OmschrijvingKolommen[-1]

        if (i < laatste_i) {
          Soortenlijst <- Soortenlijst %>%
            group_by_(
              ~ SoortengroepID,
              ~ Criterium,
              ~ Indicator,
              .dots = OmschrijvingKolommen
            ) %>%
            summarise_(
              Soortenlijst = ~ paste(as.vector(Soortenlijst), collapse = ",  ")
            ) %>%
            ungroup()
        } else {
          Soortenlijst <- Soortenlijst %>%
            group_by_(
              ~ SoortengroepID,
              ~ Criterium,
              ~ Indicator
            ) %>%
            summarise_(
              Soortenlijst = ~ paste(as.vector(Soortenlijst), collapse = ",  ")
            ) %>%
            ungroup()
        }

      }


      paste2 <- function(...,sep=", ") {
        L <- list(...)
        L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
        gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep = sep)))))
      }

      Habitatfiche <- Selectiegegevens %>%
        left_join(Habitatkarakteristieken %>% mutate_(SoortengroepID = ~NULL),
                  by = c("Indicator_habitatID" = "Indicator_habitatID")) %>%
        left_join(Soortenlijst %>%
                    select_(
                      ~ SoortengroepID,
                      ~Soortenlijst
                    ),
                  by = c("SoortengroepID" = "SoortengroepID")) %>%
        mutate_(
          Beschrijving = ~ paste2(Beschrijving, Soortenlijst, Beschrijving_naSoorten, sep = " ")
        ) %>%
        left_join(Beoordelingsmatrix,
                  by = c("Indicator_beoordelingID" = "Indicator_beoordelingID"),
                  suffix = c(".habitat", ".beoordeling")) %>%
        select_(~Versie, ~Habitattype, ~Habitatnaam, ~Habitatsubtype,
                ~Habitatsubtypenaam, ~HabitatsubtypeOmschrijving, ~Criterium.habitat,
                ~Indicator.habitat, ~Beschrijving, ~Maatregelen,
                ~Opmerkingen.habitat, ~Referenties.habitat, ~Soortenlijst,
                Beoordeling = ~Beoordeling_letterlijk, ~Criterium.beoordeling,
                ~Indicator.beoordeling, ~Opmerkingen.beoordeling,
                ~Referenties.beoordeling, ~Kwaliteitsniveau)
    } else {

      Habitatfiche <- Selectiegegevens %>%
        left_join(Habitatkarakteristieken,
                  by = c("Indicator_habitatID" = "Indicator_habitatID")) %>%
        mutate_(
          Beschrijving = ~ paste(Beschrijving, Beschrijving_naSoorten, sep = " ")
        ) %>%
        left_join(Beoordelingsmatrix,
                  by = c("Indicator_beoordelingID" = "Indicator_beoordelingID"),
                  suffix = c(".habitat", ".beoordeling")) %>%
        mutate_(Soortenlijst = ~NA) %>%
        select_(~Versie, ~Habitattype, ~Habitatnaam, ~Habitatsubtype,
                ~Habitatsubtypenaam, ~HabitatsubtypeOmschrijving, ~Criterium.habitat,
                ~Indicator.habitat, ~Beschrijving, ~Maatregelen,
                ~Opmerkingen.habitat, ~Referenties.habitat, ~Soortenlijst,
                Beoordeling = ~Beoordeling_letterlijk, ~Criterium.beoordeling,
                ~Indicator.beoordeling, ~Opmerkingen.beoordeling,
                ~Referenties.beoordeling, ~Kwaliteitsniveau)
    }


    if (Stijl[1] == "tekst") {
      Habitatfiche$Habitatnaam <- gsub("_","",Habitatfiche$Habitatnaam)
      Habitatfiche$Habitatsubtypenaam <-
        gsub("_","",Habitatfiche$Habitatsubtypenaam)
      Habitatfiche$HabitatsubtypeOmschrijving <-
        gsub("_","",Habitatfiche$HabitatsubtypeOmschrijving)
      Habitatfiche$Beschrijving <- gsub("_","",Habitatfiche$Beschrijving)
      Habitatfiche$Maatregelen <-
        gsub("_","",Habitatfiche$Maatregelen)
      Habitatfiche$Opmerkingen.habitat <-
        gsub("_","",Habitatfiche$Opmerkingen.habitat)
      Habitatfiche$Soortenlijst <- gsub("_","",Habitatfiche$Soortenlijst)
      Habitatfiche$Opmerkingen.beoordeling <-
        gsub("_","",Habitatfiche$Opmerkingen.beoordeling)
      Habitatfiche$Beoordeling_letterlijk <-
        gsub("_","",Habitatfiche$Beoordeling)
    }

    return(Habitatfiche)

  }
