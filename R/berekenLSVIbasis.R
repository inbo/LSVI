#' @title Berekent de LSVI op basis van VoorwaardeID en opgegeven waarden
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding en biotische indices op basis van gegevens, die in het juiste formaat moeten aangeleverd worden.  Zie hiervoor de beschrijving bij de parameters ('Arguments') en de tabellen van het voorbeeld.  In principe is enkel de parameter Data_habitat verplicht om op te geven, maar extra datasets zijn uiteraard wel nodig om een resultaat te bekomen.  Welke datasets relevant zijn, is afhankelijk van de opgegeven habitattypes: voor een aantal habitattypes kan een tabel met observaties en hun bedekking of aanwezigheid (=parameter 'Data_soortenKenmerken') volstaan, voor bossen zijn bv. bijkomend gegevens nodig over dood hout.
#'
#' De Lokale Staat van Instandhouding wordt weergegeven in de kolom 'Status' met als mogelijke waarden TRUE (= gunstig) en FALSE (= ongunstig).
#'
#' De biotische indices zijn afgeleid van het verschil tussen een geobserveerde waarde en de referentiewaarde voor elke indicator. Deze verschillen werden herschaald tussen +1 en -1, waarbij een positieve en negatieve waarde overeenkomt met respectievelijk een gunstige en ongunstige score. Deze verschilscores per indicator worden geaggregeerd, eerst voor de indicatoren die tot eenzelfde criterium behoren, vervolgens worden deze geaggregeerde scores verder geaggregeerd om tot een globale index te komen. Er worden drie verschillende globale indices berekend waarbij de naamgeving aangeeft welk aggregatie achtereenvolgens gebruikt werd: index_min_min, index_min_harm en index_harm_harm. Een naam met "min" duidt op minimum van de scores als aggregatie; bij "harm" werd het harmonisch gemiddelde berekend.
#'
#' @inheritParams selecteerIndicatoren
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening gemaakt wordt, bv. "Versie 2.0" of "Versie 3".  Bij de default "alle" wordt de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de streefwaarde (uiteindelijk niet opgenomen in rapport).  De betekenissen van de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de tabel Versie in de databank en kan opgevraagd met de functie geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk kwaliteitsniveau de berekening gemaakt moet worden.  (Strikt genomen is de berekening van de LSVI de berekening volgens kwaliteitsniveau 1.)
#' @param Data_habitat Een opsomming van de te analyseren opnamen met opgave van het aanwezige habitattype (= het habitattype volgens welke criteria de beoordeling moet gebeuren).  Deze info moet doorgegeven worden in de vorm van een dataframe met minimum de velden ID en Habitattype, waarbij ID een groeperende variabele is voor een opname (plaats en tijdstip).  Habitattype moet overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door geefUniekeWaarden("Habitattype", "Code")).  Eventuele extra velden zullen overgenomen worden bij de uitvoer.
#' @param Data_voorwaarden Gegevens over de opgemeten indicatoren in de vorm van een data.frame met velden ID, Criterium, Indicator, Voorwaarde, Waarde, Type, Invoertype en Eenheid, waarbij ID de groeperende variabele voor een opname is die ook bij Data_habitat opgegeven is.  Criterium, Indicator en Voorwaarde moeten overeenkomen met de waarde in de databank (op te zoeken via de functie geefInvoervereisten()).  Waarde is de waarde die voor die voorwaarde geobserveerd of gemeten is en Type het soort variabele (zie geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).  Ingeval van een categorische variabele moet bij Invoertype de naam van de lijst opgegeven worden waaruit deze waarde komt (bv. welke schaal gebruikt is, zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#' @param Data_soortenKenmerken Gegevens van soorten en kenmerken en hun bedekking (m.a.w. enkel kenmerken waarvan een bedekking gemeten is, horen in deze tabel).  Deze dataframe moet de velden ID, Vegetatielaag, Kenmerk, TypeKenmerk, Waarde, Type, Invoertype en Eenheid bevatten, waarbij ID de groeperende variabele voor een opname is die ook bij Data_habitat opgegeven is.  Kenmerk bevat een soortnaam of een naam die voorkomt in de lijst gegenereerd door geefUniekeWaarden("StudieItem", "Waarde") en TypeKenmerk geeft een beschrijving voor dat kenmerk: 'studiegroep', 'soort_Latijn', 'soort_NL' of 'soort_NBN'.  Waarde is de geobserveerde bedekking en Type het soort variabele dat voor de bedekking gebruikt is (zie geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).  Ingeval van een categorische variabele moet bij Invoertype de naam van de lijst opgegeven worden welke schaal gebruikt is (zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#' @param LIJST Dataframe met lijst die weergeeft hoe de vertaling moet gebeuren van categorische variabelen naar numerieke waarden (en omgekeerd).  Default worden deze waarden uit de databank met LSVI-indicatoren gehaald d.m.v. de functie vertaalInvoerInterval().  Aangeraden wordt om deze default te gebruiken (dus parameter niet expliciet invullen), of deze waar nodig aan te vullen met eigen schalen.  Omdat er ook een omzetting moet gebeuren voor grenswaarden uit de databank, kan het niet doorgeven van een gedeelte van deze lijst problemen geven.
#' @param na.rm Hier geeft je aan hoe de berekening moet omgaan met NA waarden. Default is FALSE. Dit betekent dat NA waarden niet worden verwijderd. Hierdoor zal de indexberekening resulteren in een NA zodra één van de indicatoren NA is. Voor de berekening van de status zal dit enkel resulteren in een NA indien minstens één van de indicatoren NA is en minstens één van de indicatoren status TRUE (= gunstig) heeft. Indien na.rm = TRUE worden eventuele NA waarden verwijderd zodat status en de indices een resultaat hebben. Doordat deze dan mogelijk niet op de volledige set van indicatoren gebaseerd zijn, moet hiermee rekening gehouden worden afhankelijk van de context waarvoor de resultaten gebruikt worden.
#'
#' @return Deze functie genereert de resultaten in de vorm van een list met 4 tabellen: een eerste met de beoordelingen per kwaliteitsniveau, een tweede met de beoordelingen per criterium en kwaliteitsniveau, een derde met de beoordelingen per indicator en kwaliteitsniveau, en een vierde met de detailgegevens inclusief meetwaarden.
#'
#' @examples
#' library(LSVI)
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
#' @importFrom dplyr %>% select distinct filter mutate row_number rename left_join summarise group_by ungroup rowwise bind_rows arrange
#' @importFrom tidyr unnest
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang .data
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
        Voorwaarde = character(),
        Waarde = character(),
        Type = character(),
        WaardeMin = double(),
        WaardeMax = double(),
        stringsAsFactors = FALSE
      ),
    Data_soortenKenmerken = data.frame(ID = character()),
    ConnectieLSVIhabitats = ConnectiePool,
    LIJST = geefVertaallijst(ConnectieLSVIhabitats),
    na.rm = FALSE
  ){

    #controle invoer
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )

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
      if (!is.character(Data_voorwaarden$ID)) {
        Data_voorwaarden$ID <- as.character(Data_voorwaarden$ID)
      }
      assert_that(has_name(Data_voorwaarden, "Criterium"))
      assert_that(has_name(Data_voorwaarden, "Indicator"))
      assert_that(has_name(Data_voorwaarden, "Voorwaarde"))
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
        .data$Belang,
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
        (Invoervereisten %>%
          filter(!.data$Referentiewaarde %in% Invoervereisten$Voorwaarde))[
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
      mutate(
        Voorwaarde.lower = tolower(.data$Voorwaarde)
      ) %>%
      left_join(
        Data_voorwaarden,
        by =
          c("ID", "Criterium", "Indicator", "Voorwaarde.lower" = "Voorwaarde"),
        suffix = c("", ".vw")
      ) %>%
      mutate(
        Voorwaarde.lower = NULL,
        Rijnr = row_number(.data$ID)
      )

    #niet opgegeven voorwaarden berekenen
    BerekendResultaat <- Resultaat %>%
      filter(is.na(.data$Waarde) & is.na(.data$Type)) %>%
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
          ),
        WaardeMin = unlist(.data$Berekening)["Min"],
        WaardeMax = unlist(.data$Berekening)["Max"],
        TheoretischMaximum = unlist(.data$Berekening)["TheoretischMaximum"],
        Berekening = NULL
      ) %>%
      ungroup() %>%
      mutate(
        Type = .data$TypeVariabele,
        Invoertype.vw = .data$Invoertype,
        Eenheid.vw = .data$Eenheid,
        AfkomstWaarde = "berekend",
        Waarde = NULL
      )

    BerekendResultaat <-
      BerekendResultaat %>%
      left_join(
        vertaalIntervalUitvoer(
          BerekendResultaat[
            , c("Rijnr", "Type", "WaardeMin", "WaardeMax",
                "Eenheid.vw", "Invoertype.vw")
            ],
          LIJST,
          ConnectieLSVIhabitats
        ),
        by = c("Rijnr")
      )

    Resultaat <- Resultaat %>%
      filter(!is.na(.data$Waarde) | !is.na(.data$Type)) %>%
      mutate(
        Waarde = as.character(.data$Waarde),
        AfkomstWaarde = "observatie"
      ) %>%
      bind_rows(BerekendResultaat)

    combinerenDubbeleVoorwaarden <- function(x) {
      #we gaan ervan uit dat een combinatie van meerdere voorwaarden
      #gecombineerd met AND en OR niet samen voorkomen met een complexe
      #voorwaarde bestaande uit de vergelijking van meerdere operatoren.
      #Indien wel, dan moet dit hier voorzien worden!
      Test <- x %>%
        filter(grepl("AND", .data$Combinatie) | grepl("OR", .data$Combinatie))
      if (nrow(Test) > 0) {
        warning(
          sprintf(
            "De rekenmodule is niet aangepast aan de complexe situatie in de databank die voorkomt bij BeoordelingID = %s.  Meld het probleem aan de beheerder van dit package en geef hierbij minstens deze foutmelding door", #nolint
            x$BeoordelingID
          )
        )
      }

      y <- x %>%
        mutate(
          BeginVoorwaarde = str_split_fixed(.data$Combinatie, " ", 2)[1]
        ) %>%
        filter(
          as.numeric(.data$BeginVoorwaarde) == .data$VoorwaardeID
        )
      x <- x %>%
        filter(.data$Voorwaarde == y$Referentiewaarde)
      y <- y %>%
        mutate(
          Voorwaarde =
            paste(.data$Voorwaarde, .data$Operator, x$Voorwaarde),
          TypeVariabele = x$Type,
          Invoertype = x$Invoertype.vw,
          RefMin = x$WaardeMin,
          RefMax = x$WaardeMax,
          Referentiewaarde = x$Waarde,
          Eenheid = x$Eenheid.vw,
          AfkomstWaarde =
            ifelse(
              .data$AfkomstWaarde == x$AfkomstWaarde,
              .data$AfkomstWaarde,
              paste(.data$AfkomstWaarde, x$AfkomstWaarde, sep = ", ")
            ),
          BeginVoorwaarde = NULL
        )
    }

    DubbeleVoorwaarden <- Resultaat %>%
      filter(.data$Referentiewaarde %in% Invoervereisten$Voorwaarde) %>%
      group_by(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Habitattype.y,
        .data$Criterium,
        .data$Indicator,
        .data$Beoordeling,
        .data$Kwaliteitsniveau,
        .data$BeoordelingID,
        .data$Combinatie,
        .data$ExtraBewerking
      ) %>%
      do(
        combinerenDubbeleVoorwaarden(.)
      ) %>%
      ungroup()

    Resultaat <- Resultaat %>%
      filter(!.data$Referentiewaarde %in% Invoervereisten$Voorwaarde) %>%
      bind_rows(DubbeleVoorwaarden) %>%
      mutate(
        TheoretischMaximum =
          ifelse(
            is.na(.data$TheoretischMaximum) & .data$Eenheid == "%",
            1,
            .data$TheoretischMaximum
          )
      )

    Statusberekening <-
      berekenStatus(
        Resultaat[
          , c("Rijnr", "RefMin", "RefMax", "Operator", "WaardeMin", "WaardeMax")
          ]
      )

    Verschilscores <-
      berekenVerschilscores(
        Resultaat[
          , c("Rijnr", "RefMin", "RefMax", "Operator", "WaardeMin",
              "WaardeMax", "TheoretischMaximum", "TypeVariabele")
          ]
      )

    Resultaat <- Resultaat %>%
      left_join(
        Statusberekening,
        by = c("Rijnr")
      ) %>%
      left_join(
        Verschilscores,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL,
        ExtraBewerking = NULL,
        RefMin = NULL, #in geval van categorische referentiewaarde (bv HB)
        RefMax = NULL,
        WaardeMin = NULL, #is geval van categorische waarde (bv HB)
        WaardeMax = NULL,
        TheoretischMaximum = ifelse(.data$Type == "Percentage",
                                .data$TheoretischMaximum * 100,
                                .data$TheoretischMaximum)
      ) %>%
      rename(
        TypeRefwaarde = .data$TypeVariabele,
        EenheidRefwaarde = .data$Eenheid,
        InvoertypeRevwaarde = .data$Invoertype,
        Status_voorwaarde = .data$Status,
        TypeWaarde = .data$Type,
        EenheidWaarde = .data$Eenheid.vw,
        InvoertypeWaarde = .data$Invoertype.vw
      ) %>%
      arrange(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Criterium,
        .data$Indicator
      )


    #resultaten op niveau van indicator afleiden
    Resultaat_indicator <- Resultaat %>%
      group_by(
        .data$ID,
        .data$Habitattype,   #en hier zouden extra gegevens uit Data_habitat
                             #moeten toegevoegd worden
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
          ),
        # voorwaarden AND wordt min(verschillen) OR wordt max(verschillen)
        Verschilscore = combinerenVerschilscore(
          unique(.data$Combinatie),
          .data$VoorwaardeID,
          .data$Verschilscore
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
        Status_criterium = as.logical(all(.data$Status_indicator,
                                          na.rm = na.rm)),
        #minimum van de scores tussen -1 en +1
        Index_min_criterium = min(.data$Verschilscore, na.rm = na.rm),
        #harmonisch gemiddelde van de verschilscores
        #de verschilscores worden tijdelijk herschaald naar 0 tot 1 range
        Index_harm_criterium =
          mean(((.data$Verschilscore + 1) / 2) ^ -1,
                na.rm = na.rm) ^ -1 * 2 - 1
      ) %>%
      ungroup()

    #resultaten op globaal niveau
    Resultaat_globaal <- Resultaat_criterium %>%
      group_by(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Kwaliteitsniveau
      ) %>%
      summarise(
        Status = as.logical(all(.data$Status_criterium, na.rm = na.rm)),
        #meest conservatieve index: one-out-all-out is resultaat van
        #Index_min_min < 0 #nolint
        Index_min_min = min(.data$Index_min_criterium, na.rm = na.rm),
        #iets minder conservatieve index
        Index_min_harm = mean(((.data$Index_min_criterium + 1) / 2) ^ -1,
                              na.rm = na.rm) ^ -1 * 2 - 1,
        # nog minder conservatieve index
        Index_harm_harm =
          mean(((.data$Index_harm_criterium + 1) / 2) ^ -1,
                na.rm = na.rm) ^ -1 * 2 - 1
      ) %>%
      ungroup()

    return(
      list(
        Resultaat_criterium = as.data.frame(Resultaat_criterium),
        Resultaat_indicator = Resultaat_indicator,
        Resultaat_detail = Resultaat,
        Resultaat_globaal = Resultaat_globaal
      )
    )
  }
