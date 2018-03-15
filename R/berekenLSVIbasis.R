#' @title Berekent de LSVI op basis van VoorwaardeID en opgegeven waarden
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding op basis van gegevens, die in het juiste formaat moeten aangeleverd worden.  Zie hiervoor de beschrijving bij de parameters ('Arguments') en de tabellen van het voorbeeld.  In principe is enkel de parameter Data_habitat verplicht om op te geven, maar extra datasets zijn uiteraard wel nodig om een resultaat te bekomen.  Welke datasets relevant zijn, is afhankelijk van de opgegeven habitattypes: voor een aantal habitattypes kan een tabel met observaties en hun bedekking of aanwezigheid (=parameter 'Data_soortenKenmerken') volstaan, voor bossen zijn bv. bijkomend gegevens nodig over dood hout.  
#'
#' @inheritParams selecteerIndicatoren
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening gemaakt wordt, bv. "Versie 2.0" of "Versie 3".  Bij de default "alle" wordt de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de streefwaarde (uiteindelijk niet opgenomen in rapport).  De betekenissen van de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de tabel Versie in de databank en kan opgevraagd met de functie geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk kwaliteitsniveau de berekening gemaakt moet worden.  (Strikt genomen is de berekening van de LSVI de berekening volgens kwaliteitsniveau 1.)
#' @param Data_habitat Een opsomming van de te analyseren opnamen met opgave van het aanwezige habitattype (= het habitattype volgens welke criteria de beoordeling moet gebeuren).  Deze info moet doorgegeven worden in de vorm van een dataframe met minimum de velden ID en Habitattype, waarbij ID een groeperende variabele is voor een opname (plaats en tijdstip).  Habitattype moet overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door geefUniekeWaarden("Habitattype", "Code")).  Eventuele extra velden zullen overgenomen worden bij de uitvoer.
#' @param Data_voorwaarden Gegevens over de opgemeten indicatoren in de vorm van een data.frame met velden ID, Criterium, Indicator, Voorwaarde, Waarde, Type, Invoertype en Eenheid, waarbij ID de groeperende variabele voor een opname is die ook bij Data_habitat opgegeven is.  Criterium, Indicator en Voorwaarde moeten overeenkomen met de waarde in de databank (op te zoeken via de functie geefInvoervereisten()).  Waarde is de waarde die voor die voorwaarde geobserveerd of gemeten is en Type het soort variabele (zie geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).  Ingeval van een categorische variabele moet bij Invoertype de naam van de lijst opgegeven worden waaruit deze waarde komt (bv. welke schaal gebruikt is, zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#' @param Data_soortenKenmerken Gegevens van soorten en kenmerken en hun bedekking (m.a.w. enkel kenmerken waarvan een bedekking gemeten is, horen in deze tabel).  Deze dataframe moet de velden ID, Kenmerk, TypeKenmerk, Waarde, Type, Invoertype en Eenheid bevatten, waarbij ID de groeperende variabele voor een opname is die ook bij Data_habitat opgegeven is.  Kenmerk bevat een soortnaam of een naam die voorkomt in de lijst gegenereerd door geefUniekeWaarden("LijstItem", "Waarde") en TypeKenmerk geeft een beschrijving voor dat kenmerk: 'studiegroep', 'soort_Latijn', 'soort_NL' of 'soort_NBN'.  Waarde is de geobserveerde bedekking en Type het soort variabele dat voor de bedekking gebruikt is (zie geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).  Ingeval van een categorische variabele moet bij Invoertype de naam van de lijst opgegeven worden welke schaal gebruikt is (zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#' @param LIJST Dataframe met lijst die weergeeft hoe de vertaling moet gebeuren van categorische variabelen naar numerieke waarden (en omgekeerd).  Default worden deze waarden uit de databank met LSVI-indicatoren gehaald d.m.v. de functie vertaalInvoerInterval().  Aangeraden wordt om deze default te gebruiken (dus parameter niet expliciet invullen), of deze waar nodig aan te vullen met eigen categorieën.  Omdat er ook een omzetting moet gebeuren voor grenswaarden uit de databank, kan het niet doorgeven van een gedeelte van deze lijst problemen geven.
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
  function(
    Versie = "alle",
    Kwaliteitsniveau = "alle",
    Data_habitat,
    Data_voorwaarden =
      data.frame(
        ID = character(),
        Criterium = character(),
        Indicator = character(),
        WaardeMin = double(),
        WaardeMax = double(),
        stringsAsFactors = FALSE
      ),
    Data_soortenKenmerken = data.frame(ID = character()),
    ConnectieLSVIhabitats = connecteerMetLSVIdb(),
    LIJST = geefVertaallijst(ConnectieLSVIhabitats)
  ){

    #controle invoer
    assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))

    invoercontroleVersie(Versie, ConnectieLSVIhabitats)

    invoercontroleKwaliteitsniveau(Kwaliteitsniveau, ConnectieLSVIhabitats)

    Data_habitat <-
      invoercontroleData_habitat(Data_habitat, ConnectieLSVIhabitats)

    if (nrow(Data_voorwaarden) > 0) {
      Data_voorwaarden <-
        invoercontroleData_voorwaarden(
          Data_voorwaarden,
          ConnectieLSVIhabitats,
          LIJST
        )
    } else {
      assert_that(has_name(Data_voorwaarden, "ID"))
      assert_that(has_name(Data_voorwaarden, "Criterium"))
      assert_that(has_name(Data_voorwaarden, "Indicator"))
      assert_that(has_name(Data_voorwaarden, "WaardeMin"))
      assert_that(has_name(Data_voorwaarden, "WaardeMax"))
    }

    if (nrow(Data_soortenKenmerken) > 0) {
      Data_soortenKenmerken <-
        invoercontroleData_soortenKenmerken(
          Data_soortenKenmerken,
          ConnectieLSVIhabitats,
          LIJST
        )
    } else {
      assert_that(has_name(Data_soortenKenmerken, "ID"))
    }



    #nodige info ophalen uit de databank
    Invoervereisten <-
      geefInvoervereisten(
        Versie,
        Habitattype = unique(Data_habitat$Habitattype),
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
        .data$Voorwaarde,
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
        LIJST,
        ConnectieLSVIhabitats
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


    #voorwaardegegevens koppelen aan info uit de databank
    Resultaat <-
      Data_habitat %>%
      left_join(
        Invoervereisten,
        by = c("Habitattype" = "Habitatsubtype")) %>%
      left_join(
        Data_voorwaarden,
        by = c("ID", "Criterium", "Indicator", "Voorwaarde"),
        suffix = c("", ".vw")
      ) %>%
      mutate(
        Rijnr = row_number(.data$ID)
      )

    #niet opgegeven voorwaarden berekenen
    BerekendResultaat <- Resultaat %>%
      filter(is.na(.data$Waarde)) %>%
      rowwise() %>%
      mutate(
        Berekening =
          list(
            berekenVoorwaarde(
              .data$ID,
              .data$VoorwaardeID,
              Data_soortenKenmerken,
              ConnectieLSVIhabitats,
              LIJST
            )
          )
      ) %>%
      unnest() %>%
      select(
        .data$Rijnr,
        .data$Berekening
      ) %>%
      group_by(.data$Rijnr) %>%
      summarise(
        Min = min(.data$Berekening),
        Max = max(.data$Berekening)
      ) %>%
      ungroup() %>%
      mutate(
        Samen =
          ifelse(
            .data$Min == .data$Max,
            .data$Min,
            paste(
              round(.data$Min, 2),
              round(.data$Max, 2),
              sep = " - ")
          ),
        Berekening = NULL
      )

    Resultaat <- Resultaat %>%
      left_join(
        BerekendResultaat,
        by = c("Rijnr")
      ) %>%
      mutate(
        WaardeMin =
          ifelse(
            is.na(.data$WaardeMin),
            .data$Min,
            .data$WaardeMin
          ),
        WaardeMax =
          ifelse(
            is.na(.data$WaardeMax),
            .data$Min,
            .data$WaardeMax
          ),
        Type =
          ifelse(
            is.na(.data$Waarde),
            "Berekend",
            .data$Type
          ),
        Waarde =
          ifelse(
            is.na(.data$Waarde),
            .data$Samen,
            .data$Waarde
          ),
        Min = NULL,
        Max = NULL,
        Samen = NULL
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
        Rijnr = NULL,
        ExtraBewerking = NULL,
        Referentiewaarde = NULL,
        Operator = NULL,
        Eenheid = NULL,
        TypeVariabele = NULL,
        Invoertype = NULL,
        RefMin = NULL,
        RefMax = NULL,
        WaardeMin = NULL,
        WaardeMax = NULL
      ) %>%
      rename(
        Status_voorwaarde = .data$Status,
        Invoertype = .data$Invoertype.vw,
        Eenheid = .data$Eenheid.vw
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

    odbcClose(ConnectieLSVIhabitats)

    return(
      list(
        as.data.frame(Resultaat_criterium),
        Resultaat_indicator,
        Resultaat
      )
    )
  }
