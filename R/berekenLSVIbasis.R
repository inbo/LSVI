#' @title Berekent de LSVI op basis van VoorwaardeID en opgegeven waarden
#'
#' @description Deze functie bepaalt de Lokale Staat van Instandhouding en
#' biotische indices op basis van gegevens, die in het juiste formaat moeten
#' aangeleverd worden.  Zie hiervoor de beschrijving bij de parameters
#' ('Arguments') en de tabellen van het voorbeeld.  In principe is enkel de
#' parameter Data_habitat verplicht om op te geven, maar extra datasets zijn
#' uiteraard wel nodig om een resultaat te bekomen.  Welke datasets relevant
#' zijn, is afhankelijk van de opgegeven habitattypes: voor een aantal
#' habitattypes kan een tabel met observaties en hun bedekking of aanwezigheid
#' (=parameter 'Data_soortenKenmerken') volstaan, voor bossen zijn bv.
#' bijkomend gegevens nodig over dood hout.
#'
#' De Lokale Staat van Instandhouding wordt weergegeven in de kolom 'Status'
#' met als mogelijke waarden TRUE (= gunstig) en FALSE (= ongunstig).
#'
#' De biotische indices zijn afgeleid van het verschil tussen een geobserveerde
#' waarde en de referentiewaarde voor elke indicator. Deze verschillen werden
#' herschaald tussen +1 en -1, waarbij een positieve en negatieve waarde
#' overeenkomt met respectievelijk een gunstige en ongunstige score. Deze
#' verschilscores per indicator worden geaggregeerd, eerst voor de indicatoren
#' die tot eenzelfde criterium behoren, vervolgens worden deze geaggregeerde
#' scores verder geaggregeerd om tot een globale index te komen. Er worden drie
#' verschillende globale indices berekend waarbij de naamgeving aangeeft welk
#' aggregatie achtereenvolgens gebruikt werd: index_min_min, index_min_harm en
#' index_harm_harm. Een naam met "min" duidt op minimum van de scores als
#' aggregatie; bij "harm" werd het harmonisch gemiddelde berekend.
#'
#' @inheritParams selecteerIndicatoren
#' @param Versie De versie van het LSVI-rapport op basis waarvan de berekening
#' gemaakt wordt, bv. "Versie 2.0" of "Versie 3".  Bij de default "alle" wordt
#' de LSVI volgens de verschillende versies berekend.
#' @param Kwaliteitsniveau Voor elke versie van de LSVI zijn er een of meerdere
#' kwaliteitsniveaus gedefinieerd in de databank.  Zo is er bij Versie 2.0 een
#' onderscheid gemaakt tussen goede staat (A), voldoende staat (B) en
#' gedegradeerde staat (C).  Hier duidt kwaliteitsniveau 1 de grens tussen
#' voldoende (B) en gedegradeerd (C) aan en kwaliteitsniveau 2 het onderscheid
#' tussen goed (A) en voldoende (B).  Bij Versie 3 duidt kwaliteitsniveau 1 op
#' het onderscheid tussen ongunstig en gunstig en kwaliteitsniveau 2 op de
#' streefwaarde (uiteindelijk niet opgenomen in rapport).  De betekenissen van
#' de 2 kwaliteitsniveaus voor de verschillende versies is weergegeven in de
#' tabel Versie in de databank en kan opgevraagd met de functie
#' geefVersieInfo().  Geef als parameter Kwaliteitsniveau op op basis van welk
#' kwaliteitsniveau de berekening gemaakt moet worden.  (Strikt genomen is de
#' berekening van de LSVI de berekening volgens kwaliteitsniveau 1.)
#' @param Data_habitat Een opsomming van de te analyseren opnamen met opgave
#' van het aanwezige habitattype (= het habitattype volgens welke criteria de
#' beoordeling moet gebeuren).  Deze info moet doorgegeven worden in de vorm
#' van een dataframe met minimum de velden ID en Habitattype, waarbij ID een
#' groeperende variabele is voor een opname (plaats en tijdstip).  Habitattype
#' moet overeenkomen met de naamgeving in de LSVI-databank (op te zoeken door
#' geefUniekeWaarden("Habitattype", "Code")).  Eventuele extra velden zullen
#' overgenomen worden bij de uitvoer.
#' @param Data_voorwaarden Gegevens over de opgemeten indicatoren in de vorm
#' van een data.frame met velden ID, Criterium, Indicator, Voorwaarde, Waarde,
#' Type, Invoertype en Eenheid, waarbij ID de groeperende variabele voor een
#' opname is die ook bij Data_habitat opgegeven is.  Criterium, Indicator en
#' Voorwaarde moeten overeenkomen met de waarde in de databank (op te zoeken
#' via de functie geefInvoervereisten()).  Waarde is de waarde die voor die
#' voorwaarde geobserveerd of gemeten is en Type het soort variabele (zie
#' geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).
#' Ingeval van een categorische variabele moet bij Invoertype de naam van de
#' lijst opgegeven worden waaruit deze waarde komt (bv. welke schaal gebruikt
#' is, zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).  Als
#' een indicator rechtstreeks op het veld ingeschat is, kan deze ingevoerd
#' worden door in deze tabel de kolom voorwaarde leeg te laten (wat in R
#' aangeduid wordt door NA) en als waarde "TRUE" of "FALSE" in te geven.  In
#' dit geval moeten Type, Invoertype en Eenheid niet ingevoerd worden.
#' @param Data_soortenKenmerken Gegevens van soorten en kenmerken en hun
#' bedekking (m.a.w. enkel kenmerken waarvan een bedekking gemeten is, horen
#' in deze tabel).  Deze dataframe moet de velden ID, Vegetatielaag, Kenmerk,
#' TypeKenmerk, Waarde, Type, Invoertype en Eenheid bevatten, waarbij ID de
#' groeperende variabele voor een opname is die ook bij Data_habitat opgegeven
#' is.  Kenmerk bevat een soortnaam of een naam die voorkomt in de lijst
#' gegenereerd door geefUniekeWaarden("StudieItem", "Waarde") en TypeKenmerk
#' geeft een beschrijving voor dat kenmerk: 'studiegroep', 'soort_Latijn',
#' 'soort_NL' of 'soort_NBN'.  Waarde is de geobserveerde bedekking en Type het
#' soort variabele dat voor de bedekking gebruikt is (zie
#' geefUniekeWaarden("TypeVariabele", "Naam") voor de mogelijke waarden).
#' Ingeval van een categorische variabele moet bij Invoertype de naam van de
#' lijst opgegeven worden welke schaal gebruikt is
#' (zie geefUniekeWaarden("Lijst", "Naam") voor alle mogelijkheden).
#' @param LIJST Dataframe met lijst die weergeeft hoe de vertaling moet
#' gebeuren van categorische variabelen naar numerieke waarden (en omgekeerd).
#' Default worden deze waarden uit de databank met LSVI-indicatoren gehaald
#' d.m.v. de functie vertaalInvoerInterval().  Aangeraden wordt om deze default
#' te gebruiken (dus parameter niet expliciet invullen), of deze waar nodig aan
#' te vullen met eigen schalen.  Omdat er ook een omzetting moet gebeuren voor
#' grenswaarden uit de databank, kan het niet doorgeven van een gedeelte van
#' deze lijst problemen geven.
#' @param Aggregatiemethode Keuze van de methode om tot één beoordeling per
#' criterium of per habitatvlek of meetpunt te komen. Er zijn twee opties:
#' (1) "RapportageHR": de beoordeling is gunstig als meer dan 50 procent van de
#' indicatoren gunstig zijn EN als geen enkele zeer belangrijke indicator
#' ongunstig is; (2) "1-out-all-out": de beoordeling is gunstig als alle
#' indicatoren gunstig zijn. "1-out-all-out" is default.
#' @param na.rm Hier geeft je aan hoe de berekening moet omgaan met NA waarden.
#' Default is FALSE. Dit betekent dat NA waarden niet worden verwijderd.
#' Hierdoor zal de indexberekening resulteren in een NA zodra één van de
#' indicatoren NA is. Voor de berekening van de status zal dit enkel resulteren
#' in een NA indien minstens één van de indicatoren NA is en minstens één
#' van de indicatoren status TRUE (= gunstig) heeft. Indien na.rm = TRUE worden
#' eventuele NA-waarden verwijderd zodat status en de indices een resultaat
#' hebben. Doordat deze dan mogelijk niet op de volledige set van indicatoren
#' gebaseerd zijn, moet hiermee rekening gehouden worden afhankelijk van de
#' context waarvoor de resultaten gebruikt worden.
#'
#' @return Deze functie genereert de resultaten in de vorm van een list met 4
#' tabellen: een eerste met de beoordelingen per kwaliteitsniveau, een tweede
#' met de beoordelingen per criterium en kwaliteitsniveau, een derde met de
#' beoordelingen per indicator en kwaliteitsniveau, en een vierde met de
#' detailgegevens inclusief meetwaarden.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar het voorbeeld werkt en kan zeker
#' # uitgetest worden.
#' \dontrun{
#' library(LSVI)
#' maakConnectiePool()
#' library(readr)
#' Data_habitat <-
#'   read_csv2(system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
#'     col_types = list(col_character(), col_character(),col_character()))
#' Data_voorwaarden <-
#'   read_csv2(
#'     system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI"))
#' Data_soortenKenmerken <-
#'   read_csv2(
#'     system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI"))
#' berekenLSVIbasis(Versie = "Versie 2.0",
#'                  Kwaliteitsniveau = "1", Data_habitat,
#'                  Data_voorwaarden, Data_soortenKenmerken)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% select distinct n filter mutate row_number rename
#' left_join summarise group_by ungroup rowwise bind_rows arrange transmute
#' @importFrom tidyr unnest
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang .data
#' @importFrom stringr str_split_fixed str_c
#'
#'
berekenLSVIbasis <- #nolint
  function(
    Versie = "alle",
    Kwaliteitsniveau = "alle",
    Data_habitat, #nolint
    Data_voorwaarden = #nolint
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
    Data_soortenKenmerken = data.frame(ID = character()), #nolint
    Aggregatiemethode = "1-out-all-out",
    ConnectieLSVIhabitats = NULL,
    LIJST = geefVertaallijst(ConnectieLSVIhabitats),
    na.rm = FALSE #nolint
  ) {

    #controle invoer
    if (is.null(ConnectieLSVIhabitats)) {
      if (exists("ConnectiePool")) {
        ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
      }
      if (!exists("LIJST")) {
        LIJST <- geefVertaallijst(ConnectieLSVIhabitats)
      }
    }
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint
    )

    Versie <- invoercontroleVersie(Versie, ConnectieLSVIhabitats)

    Kwaliteitsniveau <-
      invoercontroleKwaliteitsniveau(Kwaliteitsniveau, ConnectieLSVIhabitats)

    Data_habitat <- #nolint
      invoercontroleData_habitat(Data_habitat, ConnectieLSVIhabitats)

    if (nrow(Data_voorwaarden) > 0) {
      Data_voorwaarden <- #nolint
        invoercontroleData_voorwaarden(
          Data_voorwaarden,
          ConnectieLSVIhabitats,
          LIJST
        )
      data_voorwaarden_na <- Data_voorwaarden[[1]]
      data_voorwaarden_niet_na <- Data_voorwaarden[[2]]
    } else {
      data_voorwaarden_na <-
        data.frame(
          ID = character(),
          Criterium = character(),
          Indicator = character(),
          Waarde = character(),
          stringsAsFactors = FALSE
        )
      data_voorwaarden_niet_na <- Data_voorwaarden
    }

    if (nrow(data_voorwaarden_niet_na) == 0) {
      assert_that(has_name(data_voorwaarden_niet_na, "ID"))
      if (!is.character(data_voorwaarden_niet_na$ID)) {
        data_voorwaarden_niet_na$ID <- as.character(data_voorwaarden_niet_na$ID)
      }
      assert_that(has_name(data_voorwaarden_niet_na, "Criterium"))
      assert_that(has_name(data_voorwaarden_niet_na, "Indicator"))
      assert_that(has_name(data_voorwaarden_niet_na, "Voorwaarde"))
      assert_that(has_name(data_voorwaarden_niet_na, "WaardeMin"))
      assert_that(has_name(data_voorwaarden_niet_na, "WaardeMax"))
    }

    if (nrow(Data_soortenKenmerken) > 0) {
      Data_soortenKenmerken <- #nolint
        invoercontroleData_soortenKenmerken(
          Data_soortenKenmerken,
          ConnectieLSVIhabitats,
          LIJST
        )
    } else {
      assert_that(has_name(Data_soortenKenmerken, "ID"))
    }

    assert_that(is.string(Aggregatiemethode))
    if (
      !(Aggregatiemethode %in%
      c("RapportageHR", "1-out-all-out")
      )
    ) {
    stop(
      "Aggregatiemethode moet een van de volgende waarden zijn: 'RapportageHR' of '1-out-all-out'"   #nolint
    )
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
        .data$Referentiewaarde,
        .data$Operator,
        .data$Eenheid,
        .data$AnalyseVariabele, #toegevoegd voor invullen TheoretischMaximum
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


    #indicatorgegevens koppelen aan info uit de databank
    Resultaat <-
      Data_habitat %>%
      left_join(
        Invoervereisten,
        by = c("Habitattype" = "Habitatsubtype"))
    Jointest <- data_voorwaarden_na %>%
      anti_join(Resultaat, by = c("ID", "Criterium", "Indicator"))
    if (nrow(Jointest) > 0) {
      warning(
        sprintf(
          "Volgende records uit Data_voorwaarden kunnen niet gekoppeld worden aan indicatoren uit de databank omdat de criterium-indicator-combinatie niet voorkomt bij de LSVI-regels van het opgegeven habitattype: <%s>", #nolint
          Jointest %>%
            summarise(
              Record =
                str_c(
                  .data$ID,
                  .data$Criterium,
                  .data$Indicator,
                  sep = ", ",
                  collapse = "> <"
                )
            )
        )
      )
    }
    rm(Jointest)
    Resultaat <- Resultaat %>%
      left_join(
        data_voorwaarden_na %>%
          select(
            .data$ID, .data$Criterium, .data$Indicator, .data$Waarde
          ),
        by = c("ID", "Criterium", "Indicator"),
        suffix = c("", ".ind")
      )
    resultaat_opname_indicator <- Resultaat %>%
      filter(!is.na(.data$Waarde)) %>%
      mutate(
        AfkomstWaarde = "beoordeling indicator",
        Combinatie = NULL,
        VoorwaardeID = NULL,
        Voorwaarde = NULL,
        Referentiewaarde = NULL,
        Operator = NULL,
        Eenheid = NULL,
        AnalyseVariabele = NULL,
        TypeVariabele = NULL,
        Invoertype = NULL,
        RefMin = NULL,
        RefMax = NULL
      ) %>%
      distinct()

    #voorwaardegegevens koppelen aan info uit de databank
    Resultaat <-
      Resultaat %>%
      filter(is.na(.data$Waarde)) %>%
      mutate(
        Waarde = NULL,
        Voorwaarde.lower = tolower(.data$Voorwaarde)
      )
    Jointest <- data_voorwaarden_niet_na %>%
      anti_join(
        Resultaat,
        by =
          c("ID", "Criterium", "Indicator", "Voorwaarde" = "Voorwaarde.lower")
      )
    if (nrow(Jointest) > 0) {
      warning(
        sprintf(
          "Volgende records uit Data_voorwaarden kunnen niet gekoppeld worden aan indicatoren uit de databank omdat de criterium-indicator-voorwaarde-combinatie niet voorkomt bij de LSVI-regels van het opgegeven habitattype: <%s>", #nolint
          Jointest %>%
            summarise(
              Record =
                str_c(
                  .data$ID,
                  .data$Criterium,
                  .data$Indicator,
                  .data$Voorwaarde,
                  sep = ", ",
                  collapse = "> <"
                )
            )
        )
      )
    }
    rm(Jointest)
    Resultaat <-
      Resultaat %>%
      left_join(
        data_voorwaarden_niet_na,
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
      mutate(
        TheoretischMaximum = NA
      )

    if (nrow(BerekendResultaat) > 0) {
      BerekendResultaat <- BerekendResultaat %>%
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
          WaardeMin = as.numeric(unlist(.data$Berekening)["Min"]),
          WaardeMax = as.numeric(unlist(.data$Berekening)["Max"]),
          TheoretischMaximum =
            as.numeric(unlist(.data$Berekening)["TheoretischMaximum"]),
          Warnings = unlist(.data$Berekening)["Warnings"],
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

      #de warnings terug omzetten naar warnings, maar gegroepeerd per onderwerp
      if (!all(is.na(BerekendResultaat$Warnings))) {
        RecordsMetWarnings <- BerekendResultaat %>%
          filter(!is.na(.data$Warnings))
        GeenSoorten <- RecordsMetWarnings %>%
          filter(grepl("geen enkele soort opgegeven", .data$Warnings))
        if (nrow(GeenSoorten) > 0) {
          warning(
            sprintf(
              "Er is geen enkele soort opgegeven voor de opname(n) %s. Er wordt van uitgegaan dat hier geen vegetatie-opname gemaakt is en berekeningen op basis van soortenlijsten zullen resulteren in NA (not available). Geef tenminste 1 soort op (evt. met bedekking 0 procent) als er toch een opname gemaakt is",  #nolint
              str_c(unique(GeenSoorten$ID), collapse = ", ")
            )
          )
        }
        GeenKenmerken <- RecordsMetWarnings %>%
          filter(grepl("geen enkel kenmerk opgegeven", .data$Warnings))
        if (nrow(GeenKenmerken) > 0) {
          Infotekst <- GeenKenmerken %>%
            group_by(.data$Warnings) %>%
            summarise(
              Tekst =
                paste(
                  "Voor opname(n)", paste(unique(.data$ID), collapse = ", "),
                  "is er", unique(.data$Warnings)
                )
            ) %>%
            ungroup() %>%
            summarise(
              Tekst = paste(.data$Tekst, collapse = "; ")
            )
          warning(
            sprintf(
              "%s. Er wordt van uitgegaan dat er voor deze studiegroepen geen observaties uitgevoerd zijn en berekeningen op basis van deze studiegroepen zullen resulteren in NA (not available). Geef tenminste 1 kenmerk van deze studiegroep op (evt. met bedekking 0 procent) als deze studiegroep toch bestudeerd is.",  #nolint
              Infotekst$Tekst
            )
          )
        }
        AanOfAfwezigheid <- RecordsMetWarnings %>%
          filter(grepl("aan- of afwezigheid", .data$Warnings))
        if (nrow(AanOfAfwezigheid) > 0) {
          if (
            max(
              grepl("aan- of afwezigheid bedekking", AanOfAfwezigheid$Warnings)
            )
          ) {
            Tekst <-
              "kon niet voor alle voorwaarden een bedekking berekend worden."
          }
          if (
            max(
              grepl("aan- of afwezigheid aantal", AanOfAfwezigheid$Warnings)
            )
          ) {
            if (exists("Tekst")) {
              Tekst <- str_c(Tekst, "Ook")
            } else {
              Tekst <- ""
            }
            Tekst <-
              str_c(
                Tekst,
                "kon het aantal soorten dat aan een welbepaalde voorwaarde voldoet (bv. minimum een welbepaalde bedekking heeft), niet met zekerheid bepaald worden. In dit geval is het resultaat als een range weergegeven." #nolint
              )

          }
          warning(
            sprintf(
              "Voor sommige soorten of kenmerken uit opname(n) %s is enkel aan- of afwezigheid opgegeven, geen bedekking. Hierdoor %s",  #nolint
              str_c(unique(AanOfAfwezigheid$ID), collapse = ", "),
              Tekst
            )
          )
        }
        WarningMeting <- RecordsMetWarnings %>%
          filter(grepl("meting onbekend", .data$Warnings))
        if (nrow(WarningMeting) > 0) {
          warning(
            sprintf(
              "De waarde voor de voorwaarde(n) met VoorwaardeID %s kunnen niet berekend worden voor opname(n) %s. Geef de waarde voor deze voorwaarde rechtstreeks in als input van de functie 'berekenLSVIBasis' via 'Data_voorwaarden'",  #nolint
              str_c(unique(WarningMeting$VoorwaardeID), collapse = ", "),
              str_c(unique(WarningMeting$ID), collapse = ", ")
            )
          )
        }
      }

      BerekendResultaat <-
        BerekendResultaat %>%
        select(-.data$Warnings) %>%
        left_join(
          vertaalIntervalUitvoer(
            BerekendResultaat[
              , c("Rijnr", "Type", "WaardeMin", "WaardeMax",
                  "Eenheid.vw", "Invoertype.vw")
              ],
            LIJST %>%
              filter(.data$Basisschaal == TRUE),
            ConnectieLSVIhabitats
          ),
          by = c("Rijnr")
        )
    }

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
        .data$Combinatie
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
          ),
        TheoretischMaximum =
          ifelse(
            is.na(.data$TheoretischMaximum) &
              tolower(.data$TypeVariabele) == "categorie" &
              grepl("bedekking", tolower(.data$AnalyseVariabele)),
            1,
            .data$TheoretischMaximum
          ),
        TheoretischMaximum =
          ifelse(
            is.na(.data$TheoretischMaximum) &
              tolower(.data$TypeVariabele) == "ja/nee",
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
        RefMin = NULL, #in geval van categorische referentiewaarde (bv HB)
        RefMax = NULL,
        WaardeMin = NULL, #is geval van categorische waarde (bv HB)
        WaardeMax = NULL,
        AnalyseVariabele = NULL,
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
      )

    #voor de uitvoer de gegevens van de geobserveerde indicatoren toevoegen
    resultaat_detail <- Resultaat %>%
      bind_rows(resultaat_opname_indicator) %>%
      arrange(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Criterium,
        .data$Indicator
      )

    #resultaten op niveau van indicator afleiden
    resultaat_indicator <- Resultaat %>%
      group_by(
        .data$ID,
        .data$Habitattype,   #en hier zouden extra gegevens uit Data_habitat
                             #moeten toegevoegd worden
        .data$Versie,
        .data$Habitattype.y,
        .data$Criterium,
        .data$Indicator,
        .data$Beoordeling,
        .data$Belang,
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
      ungroup() %>%
      bind_rows(
        resultaat_opname_indicator %>%
          transmute(
            .data$ID,
            .data$Habitattype,   #en hier zouden extra gegevens uit Data_habitat
                                  #moeten toegevoegd worden
            .data$Versie,
            .data$Habitattype.y,
            .data$Criterium,
            .data$Indicator,
            .data$Beoordeling,
            .data$Belang,
            .data$Kwaliteitsniveau,
            .data$BeoordelingID,
            Status_indicator = as.logical(.data$Waarde),
          )
      ) %>%
      arrange(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Criterium,
        .data$Indicator
      )

    #resultaten op niveau van criterium afleiden
    resultaat_criterium <- resultaat_indicator %>%
      group_by(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Criterium,
        .data$Kwaliteitsniveau
      ) %>%
      summarise(
        # tijdelijke hulpvariabele: aantal indicatoren
        nInd = ifelse(
          na.rm,
          sum(!is.na(.data$Status_indicator)),
          n()),
        # tijdelijke hulpvariabele: aantal zb-indicatoren ongunstig
        nIndZb_ongunstig = sum(
                  ifelse(
                    .data$Belang == "zb",
                    .data$Status_indicator == FALSE,
                    0
                  ),
                  na.rm = na.rm
                  ),
        # tijdelijke hulpvariabele: aantal indicatoren gunstig
        nInd_gunstig = sum(
          .data$Status_indicator == TRUE,
          na.rm = TRUE
          ),
        # tijdelijke hulpvariabele: aantal indicatoren ongunstig
        nInd_ongunstig = sum(
          .data$Status_indicator == FALSE,
          na.rm = TRUE
          ),
        Status_criterium =
          ifelse(
            Aggregatiemethode == "1-out-all-out",
            as.logical(all(.data$Status_indicator, na.rm = na.rm)),
            ifelse(
              Aggregatiemethode == "RapportageHR",
              (
                ifelse(
                  .data$nIndZb_ongunstig > 0,
                  FALSE,
                  ifelse(
                    .data$nInd_gunstig > .data$nInd / 2.0,
                    TRUE,
                    ifelse(
                      .data$nInd_ongunstig >= .data$nInd / 2.0,
                      FALSE,
                      NA
                    )
                  )
                )
              )
            )
          ),
        Aggregatiemethode = Aggregatiemethode,
        #minimum van de scores tussen -1 en +1
        Index_min_criterium = min(.data$Verschilscore, na.rm = na.rm),
        #harmonisch gemiddelde van de verschilscores
        #de verschilscores worden tijdelijk herschaald naar 0 tot 1 range
        Index_harm_criterium =
          mean(
            (
              (.data$Verschilscore + 1) / 2
            ) ^ -1,
            na.rm = na.rm
          ) ^ -1 * 2 - 1
      ) %>%
      ungroup() %>%
      select(
        -.data$nInd,
        -.data$nIndZb_ongunstig,
        -.data$nInd_gunstig,
        -.data$nInd_ongunstig
      )

    #resultaten op globaal niveau
    resultaat_globaal_index <- resultaat_criterium %>%
      group_by(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Kwaliteitsniveau
      ) %>%
      summarise(
        Index_min_min = min(.data$Index_min_criterium, na.rm = na.rm),
        #iets minder conservatieve index
        Index_min_harm =
          mean(
            (
              (.data$Index_min_criterium + 1) / 2
            ) ^ -1,
            na.rm = na.rm
          ) ^ -1 * 2 - 1,
        # nog minder conservatieve index
        Index_harm_harm =
          mean(
            (
              (.data$Index_harm_criterium + 1) / 2
            ) ^ -1,
            na.rm = na.rm
          ) ^ -1 * 2 - 1
      ) %>%
      ungroup()

    resultaat_globaal_status <- resultaat_indicator %>%
      group_by(
        .data$ID,
        .data$Habitattype,
        .data$Versie,
        .data$Kwaliteitsniveau
      ) %>%
      summarise(
        # tijdelijke hulpvariabele: aantal indicatoren,
        nInd = ifelse(
          na.rm,
          sum(!is.na(.data$Status_indicator)),
          n()),
        # tijdelijke hulpvariabele: aantal zb-indicatoren ongunstig
        nIndZb_ongunstig = sum(
                  ifelse(
                    .data$Belang == "zb",
                    .data$Status_indicator == FALSE,
                    0
                  ),
                  na.rm = na.rm
                  ),
        # tijdelijke hulpvariabele: aantal indicatoren gunstig
        nInd_gunstig = sum(
          .data$Status_indicator == TRUE,
          na.rm = TRUE
          ),
        # tijdelijke hulpvariabele: aantal indicatoren ongunstig
        nInd_ongunstig = sum(
          .data$Status_indicator == FALSE,
          na.rm = TRUE
          ),
        Status =
          ifelse(
            Aggregatiemethode == "1-out-all-out",
            as.logical(all(.data$Status_indicator, na.rm = na.rm)),
            ifelse(
              Aggregatiemethode == "RapportageHR",
              (
                ifelse(
                  .data$nIndZb_ongunstig > 0,
                  FALSE,
                  ifelse(
                    .data$nInd_gunstig > .data$nInd / 2.0,
                    TRUE,
                    ifelse(
                      .data$nInd_ongunstig >= .data$nInd / 2.0,
                      FALSE,
                      NA
                    )
                  )
                )
              )
            )
          ),
        Aggregatiemethode = Aggregatiemethode
      ) %>%
      ungroup() %>%
      select(
        -.data$nInd,
        -.data$nIndZb_ongunstig,
        -.data$nInd_gunstig,
        -.data$nInd_ongunstig
      )

    resultaat_globaal <- resultaat_globaal_status %>%
      left_join(resultaat_globaal_index,
                by = c("ID", "Habitattype", "Versie", "Kwaliteitsniveau"))

    return(
      list(
        Resultaat_criterium = resultaat_criterium,
        Resultaat_indicator = resultaat_indicator,
        Resultaat_detail = resultaat_detail,
        Resultaat_globaal = resultaat_globaal
      )
    )
  }
