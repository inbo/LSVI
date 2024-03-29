#' @title Selecteert indicatoren LSVI op basis van de opgegeven parameters
#'
#' @description Deze hulpfunctie selecteert de indicatoren die gebruikt worden
#' voor de bepaling van de Lokale Staat van Instandhouding voor de opgegeven
#' parameters.  Ze is bedoeld om te gebruiken als bouwsteen in andere functies
#' waar de gegevens voor bijvoorbeeld een welbepaalde versie of welbepaalde
#' habitattypes geselecteerd moeten kunnen worden.
#'
#' @template Zoekparameters
#'
#' @param ConnectieLSVIhabitats Connectie met de databank met indicatoren voor
#' de LSVI van habitats, in te stellen d.m.v. functie connecteerMetLSVIdb.
#' @param Versie De versie van het LSVI-rapport, bv. "Versie 2" of "Versie 3".
#' Bij de default "alle" worden de gegevens voor de verschillende versies
#' gegeven.  De mogelijke waarden kunnen opgevraagd worden via
#' geefUniekeWaarden("Versie", "VersieLSVI") of geefVersieInfo().
#' @param Habitatgroep Parameter waarmee alle habitats van een bepaalde
#' habitatgroep kunnen geselecteerd worden, bv. "Bossen", "Heiden",
#' "(Half-)natuurlijke graslanden", "Zoete en brakke wateren",...   en "alle"
#' (=default).  Deze waarde moet niet gespecifieerd worden als een bepaald
#' habitat(sub)type geselecteerd wordt.  De mogelijke waarden kunnen opgevraagd
#' worden via geefUniekeWaarden("Habitatgroep", "Naam").
#' @param Habitattype Parameter waarmee een habitattype of habitatsubtype kan
#' geselecteerd worden.  Als dit een habitattype betreft met meerdere subtypes,
#' zullen de gegevens van alle subtypes van dit habitattype weergegeven worden.
#' De mogelijke waarden kunnen opgevraagd worden via
#' geefUniekeWaarden("Habitattype", "Code").  Er is voor deze parameter ook de
#' mogelijkheid om een vector van meerdere habitat(sub)typen op te geven.
#' @param Criterium Het LSVI-criterium waarvoor de gegevens geselecteerd
#' worden: "Vegetatie", "Structuur", "Verstoring" of "alle".
#' @param Indicator De indicator waarvoor de gegevens uit de databank gehaald
#' worden.  De mogelijke waarden kunnen opgevraagd worden via
#' geefUniekeWaarden("Indicator", "Naam").
#' @param HabitatnamenToevoegen Moeten de namen van de habitattypen en
#' habitatsubtypen toegevoegd worden als extra kolommen?  (Bij FALSE worden
#' enkel de habitatcodes toegevoegd, niet de volledige namen.)
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype,
#' Habitatsubtype, Criterium, Indicator, Indicator_habitatID, TaxongroepId en
#' Indicator_beoordelingID.
#'
#' @export
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' library(LSVI)
#' maakConnectiePool()
#' selecteerIndicatoren(Versie = "Versie 2.0", Habitattype = "4030")
#' selecteerIndicatoren(Versie = "Versie 2.0", Habitatgroep = "Heiden")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#'
#'
selecteerIndicatoren <-
  function(Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           Criterium = "alle",
           Indicator = "alle",
           HabitatnamenToevoegen = FALSE,
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

    Versie <- invoercontroleVersie(Versie, ConnectieLSVIhabitats)

    assert_that(is.string(Habitatgroep))
    Habitatgroep <- str_to_sentence(Habitatgroep)
    Habitatgroep <- ifelse(Habitatgroep == "Alle", "alle", Habitatgroep)
    controleerInvoerwaarde(
      "Habitatgroep", Habitatgroep,
      "Habitatgroep", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )

    if (is.numeric(Habitattype)) {
      Habitattype <- as.character(Habitattype)
    }
    assert_that(is.character(Habitattype))
    controleerInvoerwaarde(
      "Habitattype", Habitattype,
      "Habitattype", "Code", ConnectieLSVIhabitats, Tolower = FALSE
    )

    assert_that(is.string(Criterium))
    Criterium <- str_to_sentence(Criterium)
    Criterium <- ifelse(Criterium == "Alle", "alle", Criterium)
    controleerInvoerwaarde(
      "Criterium", Criterium,
      "Criterium", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )

    assert_that(is.string(Indicator))
    controleerInvoerwaarde(
      "Indicator", Indicator,
      "Indicator", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )

    assert_that(is.logical(HabitatnamenToevoegen))


    query_uitbreiding <- ifelse(HabitatnamenToevoegen,
                                "Ht1.Naam AS Habitatnaam,
    Ht2.Naam AS Habitatsubtypenaam,
    cast(Ht2.Omschrijving AS nvarchar(10)) AS HabitatsubtypeOmschrijving,
                                Habitatgroep.Naam AS Habitatgroepnaam, ",
                                "")

    #eerst de selectiegegevens ophalen en de nodige gegevens uit tabel
    #Indicator_habitat, query samenstellen op basis van parameters
    if (Habitattype[1] == "alle") {
      Parametervoorwaarde <- FALSE
      Join <- "INNER"
      QueryEinde <-
        "Habitatselectie.HabitatsubtypeId = Indicator_habitat.HabitattypeID"
    } else {
      Parametervoorwaarde <- TRUE
      Join <- "LEFT"
      Habitattypen <-
        paste(Habitattype, collapse = "','")
      QueryEinde <-
        sprintf(
          "(Habitatselectie.HabitatsubtypeId = Indicator_habitat.HabitattypeID
            OR Habitatselectie.HabitattypeId = Indicator_habitat.HabitattypeID)
          WHERE (Ht1.Code in ('%s') OR Ht2.Code in ('%s'))",
        Habitattypen, Habitattypen)
    }

    query <-
      sprintf(
        "WITH Habitatselectie
        AS
        (
          SELECT Ht1.Id AS HabitattypeId, Ht1.Id AS HabitatsubtypeId
          FROM Habitattype AS Ht1
          WHERE Ht1.ParentId IS NULL
        UNION ALL
          SELECT Habitatselectie.HabitattypeId, Ht2.Id AS HabitatsubtypeId
          FROM Habitatselectie INNER JOIN Habitattype AS Ht2
          ON Habitatselectie.HabitatsubtypeId = Ht2.ParentId
        )
        SELECT Versie.VersieLSVI AS Versie, Ht1.Code AS Habitattype,
            Ht2.Code AS Habitatsubtype, %s
            Criterium.Naam AS Criterium, Indicator.Naam AS Indicator,
            Indicator_habitat.Id AS Indicator_habitatID,
            Indicator_habitat.TaxongroepId,
            IndicatortabellenKoppeling.Indicator_beoordelingId
              AS Indicator_beoordelingID
        FROM Habitatselectie
          INNER JOIN Habitattype Ht1
            ON Habitatselectie.HabitattypeId = Ht1.Id
          INNER JOIN Habitattype Ht2
            ON Habitatselectie.HabitatsubtypeId = Ht2.Id
          INNER JOIN Habitatgroep ON Ht1.HabitatgroepId = Habitatgroep.Id
        %s JOIN (((Indicator_habitat
        INNER JOIN
          (Indicator INNER JOIN Criterium
            ON Indicator.CriteriumID = Criterium.Id)
        ON Indicator_habitat.IndicatorID = Indicator.Id)
        INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.Id)
        LEFT JOIN IndicatortabellenKoppeling
        ON Indicator_habitat.Id =
          IndicatortabellenKoppeling.Indicator_habitatId)
        ON %s",
        query_uitbreiding, Join, QueryEinde
      )
    if (Versie[1] != "alle") {
      if (Parametervoorwaarde) {
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <-
        sprintf("%s %s Versie.VersieLSVI = '%s'", query, Voegwoord, Versie)
    }
    if (Habitatgroep[1] != "alle") {
      if (Parametervoorwaarde) {
        Voegwoord <- "AND"
      } else {
        Voegwoord <- "WHERE"
        Parametervoorwaarde <- TRUE
      }
      query <-
        sprintf("%s %s Habitatgroep.Naam = '%s'",
                query, Voegwoord, Habitatgroep)
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
