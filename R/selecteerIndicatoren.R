#' @title Selecteert indicatoren LSVI op basis van de opgegeven parameters
#'
#' @description Deze hulpfunctie selecteert de indicatoren die gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding voor de opgegeven parameters.  Ze is bedoeld om te gebruiken als bouwsteen in andere functies waar de gegevens voor bijvoorbeeld een welbepaalde versie of welbepaalde habitattypes geselecteerd moeten kunnen worden.
#'
#' @template Zoekparameters
#'
#' @param ConnectieLSVIhabitats Connectie met de databank met indicatoren voor de LSVI van habitats, in te stellen d.m.v. functie connecteerMetLSVIdb.
#' @param Versie De versie van het LSVI-rapport, bv. "Versie 2" of "Versie 3".  Bij de default "alle" worden de gegevens voor de verschillende versies gegeven.  De mogelijke waarden kunnen opgevraagd worden via geefUniekeWaarden(ConnectieLSVIhabitats, "Versie", "VersieLSVI") of geefVersieInfo(ConnectieLSVIhabitats).
#' @param Habitatgroep Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen geselecteerd worden, bv. "Bossen", "Heiden", "(Half-)natuurlijke graslanden", "Zoete wateren",...   en "alle" (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type geselecteerd wordt.  De mogelijke waarden kunnen opgevraagd worden via geefUniekeWaarden(ConnectieLSVIhabitats, "Habitatgroep", "Habitatgroepnaam").
#' @param Habitattype Parameter waarmee een habitattype of habitatsubtype kan geselecteerd worden.  Als dit een habitattype betreft met meerdere subtypes, zullen de gegevens van alle subtypes van dit habitattype weergegeven worden.  De mogelijke waarden kunnen opgevraagd worden via geefUniekeWaarden(ConnectieLSVIhabitats, "Habitattype", "Code").  Er is voor deze parameter ook de mogelijkheid om een vector van meerdere habitat(sub)typen op te geven.
#' @param Criterium Het LSVI-criterium waarvoor de gegevens geselecteerd worden: "Vegetatie", "Structuur", "Verstoring" of "alle".
#' @param Indicator De indicator waarvoor de gegevens uit de databank gehaald worden.  De mogelijke waarden kunnen opgevraagd worden via geefUniekeWaarden(ConnectieLSVIhabitats, "Indicator", "Naam").
#' @param HabitatnamenToevoegen Moeten de namen van de habitattypen en habitatsubtypen toegevoegd worden als extra kolommen?  (Bij FALSE worden enkel de habitatcodes toegevoegd, niet de volledige namen.)
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype, Habitatsubtype, Criterium, Indicator, Indicator_habitatID, TaxongroepId en Indicator_beoordelingID.
#'
#' @examples
#' selecteerIndicatoren(Versie = "Versie 3", Habitattype = "4010")
#' selecteerIndicatoren(Versie = "Versie 3", Habitatgroep = "Heiden")
#'
#' @export
#'
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
#'
selecteerIndicatoren <-
  function(Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           Criterium = "alle",
           Indicator = "alle",
           HabitatnamenToevoegen = FALSE,
           ConnectieLSVIhabitats = ConnectiePool){

    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )

    invoercontroleVersie(Versie, ConnectieLSVIhabitats)

    assert_that(is.string(Habitatgroep))
    if (
      !(Habitatgroep %in%
        geefUniekeWaarden("Habitatgroep", "Naam", ConnectieLSVIhabitats))
    ) {
      stop(
        sprintf(
          "Habitatgroep moet een van de volgende waarden zijn: %s",
          geefUniekeWaarden("Habitatgroep", "Naam", ConnectieLSVIhabitats)
        )
      )
    }


    if (is.numeric(Habitattype)) {
      Habitattype <- as.character(Habitattype)
    }
    assert_that(is.character(Habitattype))
    if (
      !all(Habitattype %in%
        geefUniekeWaarden("Habitattype", "Code", ConnectieLSVIhabitats))
    ) {
      Habitattypen <-
        paste(
          geefUniekeWaarden("Habitattype", "Code", ConnectieLSVIhabitats),
          collapse = ", "
        )
      stop(
        sprintf(
          "De opgegeven habitattypen mogen enkel de volgende waarden zijn: %s",
          Habitattypen
        )
      )
    }

    assert_that(is.string(Criterium))
    if (
      !(Criterium %in%
        geefUniekeWaarden("Criterium", "Naam", ConnectieLSVIhabitats))
    ) {
      stop(
        sprintf(
          "Criterium moet een van de volgende waarden zijn: %s",
          geefUniekeWaarden("Criterium", "Naam", ConnectieLSVIhabitats)
        )
      )
    }

    assert_that(is.string(Indicator))
    if (
      !(Indicator %in%
        geefUniekeWaarden("Indicator", "Naam", ConnectieLSVIhabitats))
    ) {
      stop(
        sprintf(
          "Indicator moet een van de volgende waarden zijn: %s",
          geefUniekeWaarden("Indicator", "Naam", ConnectieLSVIhabitats)
        )
      )
    }

    assert_that(is.logical(HabitatnamenToevoegen))


    query_uitbreiding <- ifelse(HabitatnamenToevoegen,
                                "Habitatselectie.Habitatnaam,
    Habitatselectie.Habitatsubtypenaam,
    Habitatselectie.HabitatsubtypeOmschrijving,
                                Habitatselectie.Habitatgroepnaam, ",
                                "")

    #eerst de selectiegegevens ophalen en de nodige gegevens uit tabel
    #Indicator_habitat, query samenstellen op basis van parameters
    Parametervoorwaarde <- FALSE
    query <-
      sprintf(
        "WITH Habitatselectie
        AS
        (
          SELECT Habitatgroep.Naam AS Habitatgroepnaam,
            Ht1.Code AS Habitattype, Ht1.Naam AS Habitatnaam,
            Ht1.Code AS Habitatsubtype, Ht1.Naam AS Habitatsubtypenaam,
            Ht1.Omschrijving AS HabitatsubtypeOmschrijving,
            Ht1.Id AS HabitattypeId
          FROM Habitattype AS Ht1 INNER JOIN Habitatgroep
          ON Ht1.HabitatgroepId = Habitatgroep.Id
          WHERE Ht1.ParentId IS NULL
        UNION ALL
          SELECT Habitatselectie.Habitatgroepnaam,
            Habitatselectie.Habitattype, Habitatselectie.Habitatnaam,
            Ht2.Code AS Habitatsubtype, Ht2.Naam AS Habitatsubtypenaam,
            Ht2.Omschrijving AS HabitatsubtypeOmschrijving,
            Ht2.Id AS HabitattypeId
          FROM Habitatselectie INNER JOIN Habitattype AS Ht2
          ON Habitatselectie.HabitattypeId = Ht2.ParentId
        )
        SELECT Versie.VersieLSVI AS Versie, Habitatselectie.Habitattype,
            Habitatselectie.Habitatsubtype, %s
            Criterium.Naam AS Criterium, Indicator.Naam AS Indicator,
            Indicator_habitat.Id AS Indicator_habitatID,
            Indicator_habitat.TaxongroepId,
            IndicatortabellenKoppeling.Indicator_beoordelingID
        FROM (((Indicator_habitat
        INNER JOIN Habitatselectie
        ON Indicator_habitat.HabitattypeID = Habitatselectie.HabitattypeId)
        INNER JOIN
          (Indicator INNER JOIN Criterium
            ON Indicator.CriteriumID = Criterium.Id)
        ON Indicator_habitat.IndicatorID = Indicator.Id)
        INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.Id)
        LEFT JOIN IndicatortabellenKoppeling
        ON Indicator_habitat.Id =
          IndicatortabellenKoppeling.Indicator_habitatId",
        query_uitbreiding
      )
    if (Versie[1] != "alle") {
      query <- sprintf("%s WHERE Versie.VersieLSVI = '%s'", query, Versie)
      Parametervoorwaarde <- TRUE
    }
    if (Habitatgroep[1] != "alle") {
      if (Parametervoorwaarde) {
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <-
        sprintf("%s %s Habitatselectie.Habitatgroepnaam = '%s'",
                query, Voegwoord, Habitatgroep)
    }
    if (Habitattype[1] != "alle") {
      if (Parametervoorwaarde) {
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      Habitattypen <-
        paste(Habitattype, collapse = "','")
      query <-
        sprintf("%s %s (Habitatselectie.Habitattype in ('%s') OR
                Habitatselectie.Habitatsubtype in ('%s'))",
                query, Voegwoord, Habitattypen, Habitattypen)
    }
    if (Criterium[1] != "alle") {
      if (Parametervoorwaarde) {
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <-
        sprintf("%s %s Criterium.Naam = '%s'", query, Voegwoord, Criterium)
    }
    if (Indicator[1] != "alle") {
      if (Parametervoorwaarde) {
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <-
        sprintf("%s %s Indicator.Naam = '%s'", query, Voegwoord, Indicator)
    }

    Selectiegegevens <-
      dbGetQuery(ConnectieLSVIhabitats, query) %>%
      mutate(
        Habitattype =
          ifelse(
            rep(is.numeric(.data$Habitattype), length(.data$Habitattype)),
            as.character(.data$Habitattype),
            .data$Habitattype
          ),
        Habitatsubtype =
          ifelse(
            rep(is.numeric(.data$Habitatsubtype), length(.data$Habitatsubtype)),
            as.character(.data$Habitatsubtype),
            .data$Habitatsubtype
          )
      )


    return(Selectiegegevens)

  }
