#' @title Berekent de LSVI op basis van VoorwaardeID en opgegeven waarden
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding op basis van een opgegeven tabel met VoorwaardeID en een opgegeven waarde, die in het juiste formaat moet zijn.  Zie voor meer informatie hierover onder Data_voorwaarden.
#'
#' @inheritParams selecteerIndicatoren
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening gemaakt wordt, bv. "Versie 2" of "Versie 3".  Bij de default "alle" wordt de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de streefwaarde.  De betekenissen van de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de tabel Versie in de databank en kan opgevraagd met de functie geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk kwaliteitsniveau de berekening gemaakt moet worden (strikt genomen is de berekening van de LSVI de berekening volgens kwaliteitsniveau 1).
#' @param Data_habitat Een opsomming van de te analyseren opnamen met opgave van het aanwezige habitattype (= het habitattype volgens welke criteria de beoordeling moet gebeuren).  Deze info moet doorgegeven worden in de vorm van een dataframe met minimum de velden ID en Habitattype, waarbij ID een groeperende variabele is voor een opname (plaats en tijdstip).  Habitattype moet overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door geefUniekeWaarden("Habitattype", "Code")).  Eventuele extra velden zullen overgenomen worden bij de uitvoer.
#' @param Data_voorwaarden Gegevens over de opgemeten indicatoren in de vorm van een data.frame met velden ID, Criterium, Indicator, Voorwaarde, Waarde, Type, Invoertype en Eenheid, waarbij ID de groeperende variabele voor een opname is die ook bij Data_habitat opgegeven is.  Criterium, Indicator en Voorwaarde moeten overeenkomen met de waarde in de databank (op te zoeken via de functie geefInvoervereisten(), waarbij Voorwaarde moet overeenkomen met 'VoorwaardeNaam').  Waarde is de waarde die voor die voorwaarde geobserveerd of gemeten is en Type het soort variabele (zie geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).  Ingeval van een categorische variabele moet bij Invoertype de naam van de lijst opgegeven worden waaruit deze waarde komt (bv. welke schaal gebruikt is, zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#' @param Data_soortenKenmerken Gegevens van soorten en kenmerken en hun bedekking (m.a.w. enkel kenmerken waarvan een bedekking gemeten is, horen in deze tabel).  Deze dataframe moet de velden ID, Kenmerk, TypeKenmerk, Waarde, Type, Invoertype en Eenheid bevatten, waarbij ID de groeperende variabele voor een opname is die ook bij Data_habitat opgegeven is.  Kenmerk bevat een soortnaam of een naam die voorkomt in de lijst gegenereerd door geefUniekeWaarden("LijstItem", "Waarde") en TypeKenmerk geeft een beschrijving voor dat kenmerk: 'studiegroep', 'soort_Latijn', 'soort_NL' of 'soort_NBN'.  Waarde is de geobserveerde bedekking en Type het soort variabele dat voor de bedekking gebruikt is (zie geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).  Ingeval van een categorische variabele moet bij Invoertype de naam van de lijst opgegeven worden welke schaal gebruikt is (zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#'
#'
#' @return Deze functie genereert de resultaten in de vorm van een list met 3 tabellen: een eerste met de beoordelingen per criterium en kwaliteitsniveau, een tweede met de beoordelingen per indicator en kwaliteitsniveau, en een derde met de detailgegevens inclusief meetwaarden.
#'
#' @examples
#' library(readr)
#' Data_habitat <-
#'     read_csv2(system.file("vbdata/opname4030habitat.csv", package = "LSVI"),
#'               col_types = list(col_character(), col_character(),col_character()))
#' Data_voorwaarden <-
#'     read_csv2(system.file("vbdata/opname4030voorwaarden.csv", package = "LSVI"))
#' Data_soortenKenmerken <-
#'     read_csv2(system.file("vbdata/opname4030soortenKenmerken.csv", package = "LSVI"))
#' berekenLSVIbasis(Versie = "Versie 3",
#'                  Kwaliteitsniveau = "1", Data_habitat,
#'                  Data_voorwaarden, Data_soortenKenmerken)
#'
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% select distinct filter mutate row_number rename left_join summarise group_by ungroup bind_rows
#' @importFrom assertthat assert_that has_name
#' @importFrom pander evals
#'
#'
berekenLSVIbasis <-
  function(Versie = "alle",
           Kwaliteitsniveau = "alle",
           Data_habitat,
           Data_voorwaarden,
           Data_soortenKenmerken,
           ConnectieLSVIhabitats = connecteerMetLSVIdb(),
           ConnectieNBN = connecteerMetLSVIdb(Databank = "D0017_00_NBNData"),
           LIJST = geefVertaallijst(ConnectieLSVIhabitats)){

    #controle invoer
    assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))
    assert_that(inherits(ConnectieNBN, "RODBC"))

    assert_that(is.string(Versie))
    if (
      !(Versie %in%
        geefUniekeWaarden("Versie", "VersieLSVI", ConnectieLSVIhabitats)
      )
    ) {
      stop(
        sprintf(
          "Versie moet een van de volgende waarden zijn: %s",
          geefUniekeWaarden("Versie", "VersieLSVI", ConnectieLSVIhabitats)
        )
      )
    }

    Kwaliteitsniveau <- ifelse(Kwaliteitsniveau == 1, "1",
                               ifelse(Kwaliteitsniveau == 2, "2",
                                      Kwaliteitsniveau))
    assert_that(is.string(Kwaliteitsniveau))
    if (!(Kwaliteitsniveau %in%
          geefUniekeWaarden(
            "Beoordeling",
            "Kwaliteitsniveau",
            ConnectieLSVIhabitats
          )
        )) {
      stop(
        sprintf(
          "Kwaliteitsniveau moet een van de volgende waarden zijn: %s",
          geefUniekeWaarden(
            "Beoordeling",
            "Kwaliteitsniveau",
            ConnectieLSVIhabitats
          )
        )
      )
    }

    assert_that(inherits(Data_habitat, "data.frame"))
    assert_that(has_name(Data_habitat, "ID"))
    assert_that(has_name(Data_habitat, "Habitattype"))
    if (!all(Data_habitat$Habitattype %in%
             geefUniekeWaarden("Habitattype", "Code", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_habitat$Habitattype komen overeen met waarden vermeld in de databank.") #nolint
    }

    assert_that(inherits(Data_voorwaarden, "data.frame"))
    assert_that(has_name(Data_voorwaarden, "ID"))
    assert_that(has_name(Data_voorwaarden, "Criterium"))
    if (!all(Data_voorwaarden$Criterium %in%
            geefUniekeWaarden("Criterium", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Criterium komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Indicator"))
    if (!all(Data_voorwaarden$Indicator %in%
             geefUniekeWaarden("Indicator", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Indicator komen overeen met waarden vermeld in de databank.") #nolint
    }
    #misschien best ook testen dat die indicator-criterium-combinatie in de databank voorkomt?  En of deze voor dat habitattype voorkomt, maar dat best verderop doen
    #Voorwaarde ook verplichten?  Anders wel testen of het ok is als het aanwezig is.
    assert_that(has_name(Data_voorwaarden, "Waarde"))
    assert_that(has_name(Data_voorwaarden, "Type"))
    if (
      !all(
        Data_voorwaarden$Type %in%
          geefUniekeWaarden("TypeVariabele", "Naam", ConnectieLSVIhabitats)
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Type komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Invoertype"))
    if (!all(is.na(Data_voorwaarden$Invoertype) |
             Data_voorwaarden$Invoertype %in%
             geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Eenheid"))
    if (
      !all(
        Data_voorwaarden$Eenheid %in%
           geefUniekeWaarden(
             "AnalyseVariabele",
             "Eenheid",
             ConnectieLSVIhabitats
           )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Eenheid komen overeen met waarden vermeld in de databank.") #nolint
    }

    assert_that(inherits(Data_soortenKenmerken, "data.frame"))
    assert_that(has_name(Data_soortenKenmerken, "ID"))
    assert_that(has_name(Data_soortenKenmerken, "Kenmerk"))
    assert_that(has_name(Data_soortenKenmerken, "TypeKenmerk"))
    #hier moet nog controle op gebeuren!!!
    assert_that(has_name(Data_soortenKenmerken, "Waarde"))
    assert_that(has_name(Data_soortenKenmerken, "Type"))
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
    if (!all(is.na(Data_soortenKenmerken$Invoertype) |
             Data_soortenKenmerken$Invoertype %in%
             geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_soortenKenmerken, "Eenheid"))
    if (
      !all(
        Data_soortenKenmerken$Eenheid %in%
          geefUniekeWaarden(
            "AnalyseVariabele",
            "Eenheid",
            ConnectieLSVIhabitats
          )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Eenheid komen overeen met waarden vermeld in de databank.") #nolint
    }


    #nodige info ophalen uit de databank
    Invoervereisten <-
      geefInvoervereisten(
        Versie,
        Habitattype = unique(Data_habitat$Habitattype),  #selecteerIndicatoren zou aangepast moeten worden om dit toe te laten!
        Kwaliteitsniveau = Kwaliteitsniveau,
        ConnectieLSVIhabitats = ConnectieLSVIhabitats
      ) %>%
      select(
        .data$Versie,
        .data$Habitattype,
        .data$Habitatsubtype,
        .data$Criterium,
        .data$Indicator,
        .data$Beoordeling,
        .data$Kwaliteitsniveau,
        .data$BeoordelingID,
        .data$Combinatie,
        .data$VoorwaardeID,
        .data$VoorwaardeNaam,
        .data$ExtraBewerking,
        .data$Referentiewaarde,
        .data$Operator,
        .data$Eenheid,
        .data$TypeVariabele,
        .data$Invoertype
      ) %>%
      distinct() %>%
      filter(!is.na(.data$TypeVariabele)) %>%
      mutate(
        Rijnr = row_number(.data$VoorwaardeID)
      )

    IntervalVereisten <-
      vertaalInvoerInterval(
        Invoervereisten[
          , c("Rijnr", "TypeVariabele", "Referentiewaarde",
              "Eenheid", "Invoertype")
        ],
        LIJST
      ) %>%
      rename(
        RefMin = .data$Min,
        RefMax = .data$Max
      )

    Invoervereisten <- Invoervereisten %>%
      left_join(
        IntervalVereisten,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL
      )


    #ingevoerde voorwaarden omzetten naar interval
    Data_voorwaarden <- Data_voorwaarden %>%
      mutate(
        Rijnr = row_number(.data$ID)
      )

    IntervalVoorwaarden <-
      vertaalInvoerInterval(
        Data_voorwaarden[
          , c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")
        ],
        LIJST
      ) %>%
      rename(
        WaardeMin = .data$Min,
        WaardeMax = .data$Max
      )

    Data_voorwaarden <- Data_voorwaarden %>%
      left_join(
        IntervalVoorwaarden,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL
      )


    #voorwaardegegevens koppelen aan info uit de databank
    #en niet opgegeven voorwaarden berekenen
    Resultaat <-
      Data_habitat %>%
      left_join(
        Invoervereisten,
        by = c("Habitattype" = "Habitatsubtype")) %>%
      left_join(
        Data_voorwaarden,
        by = c("ID", "Criterium", "Indicator"),
        suffix = c("", ".vw")
      ) %>%
      rowwise() %>%
      mutate(
        Berekening =           #is in principe enkel nodig als .data$Waarde NA is
          list(
            berekenVoorwaarde(
              .data$ID,
              .data$VoorwaardeID,
              Data_soortenKenmerken,
              ConnectieLSVIhabitats,
              ConnectieNBN,
              LIJST
            )
          ),
        WaardeMin =
          ifelse(
            is.na(.data$WaardeMin),
            .data$Berekening[[1]],
            .data$WaardeMin
          ),
        WaardeMax =
          ifelse(
            is.na(.data$WaardeMax),
            .data$Berekening[[2]],
            .data$WaardeMax
          ),
        Berekening = NULL
      ) %>%
      ungroup() %>%
      mutate(
        Rijnr = row_number(.data$ID)
      )

    Statusberekening <-
      berekenStatus(
        Resultaat[
          , c("Rijnr", "RefMin", "RefMax", "Operator", "WaardeMin", "WaardeMax")
        ]
      )

    Resultaat <- Resultaat %>%
      left_join(
        Statusberekening,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL
      ) %>%
      rename(
        Status_voorwaarde = .data$Status
      )


    #resultaten op niveau van indicator afleiden
    Resultaat_indicator <- Resultaat %>%
      group_by(
        .data$ID,
        .data$Habitattype,   #en hier zouden extra gegevens uit Data_habitat moeten toegevoegd worden
        .data$Versie,
        .data$Habitattype.y,
        .data$Criterium,
        .data$Indicator,
        .data$Beoordeling,
        .data$Kwaliteitsniveau,
        .data$BeoordelingID
      ) %>%
      summarise(
        Status_indicator =
          combinerenVoorwaarden(
            unique(.data$Combinatie),
            .data$VoorwaardeID,
            .data$Status_voorwaarde
          )
      ) %>%
      ungroup()

    #resultaten op niveau van criterium afleiden
    Resultaat_criterium <- Resultaat_indicator %>%
      group_by(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Criterium,
        .data$Kwaliteitsniveau
      ) %>%
      summarise(
        Status_criterium = as.logical(all(.data$Status_indicator))
      ) %>%
      ungroup()


    return(
      list(
        as.data.frame(Resultaat_criterium),
        Resultaat_indicator,
        Resultaat
      )
    )
  }
