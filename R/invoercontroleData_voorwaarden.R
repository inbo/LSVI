#' Invoercontrole voor dataframe Data_voorwaarden
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).  Ingeval van Data_voorwaarden is ook de omzetting van de voorwaarden naar een interval opgenomen in de functie.
#'
#' @param Data_voorwaarden dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate row_number rename left_join
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#' 
#' @export
#'
invoercontroleData_voorwaarden <-
  function(Data_voorwaarden, ConnectieLSVIhabitats, LIJST) {
    assert_that(inherits(Data_voorwaarden, "data.frame"))
    assert_that(has_name(Data_voorwaarden, "ID"))
    if (!is.character(Data_voorwaarden$ID)) {
      Data_voorwaarden$ID <- as.character(Data_voorwaarden$ID)
    }
    assert_that(has_name(Data_voorwaarden, "Criterium"))
    if (!is.character(Data_voorwaarden$Criterium)) {
      Data_voorwaarden$Criterium <- as.character(Data_voorwaarden$Criterium)
    }
    Data_voorwaarden$Criterium <- str_to_sentence(Data_voorwaarden$Criterium)
    controleerInvoerwaarde(
      "Data_voorwaarden$Criterium", Data_voorwaarden$Criterium,
      "Criterium", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )
    assert_that(has_name(Data_voorwaarden, "Indicator"))
    if (!is.character(Data_voorwaarden$Indicator)) {
      Data_voorwaarden$Indicator <- as.character(Data_voorwaarden$Indicator)
    }
    controleerInvoerwaarde(
      "Data_voorwaarden$Indicator", Data_voorwaarden$Indicator,
      "Indicator", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
    )
    assert_that(has_name(Data_voorwaarden, "Voorwaarde"))
    if (!is.character(Data_voorwaarden$Voorwaarde)) {
      Data_voorwaarden$Voorwaarde <- as.character(Data_voorwaarden$Voorwaarde)
    }
    uitbreidingTolower <- function(x) {
      tryCatch(
        tolower(x),
        error = function(e) {
          x
          Encoding(x) <- "latin1"
          z <- tolower(x)
          return(z)
        }
      )
    }
    Data_voorwaarden$Voorwaarde <-
      uitbreidingTolower(Data_voorwaarden$Voorwaarde)
    controleerInvoerwaarde(
      "Data_voorwaarden$Voorwaarde",
      Data_voorwaarden$Voorwaarde[!is.na(Data_voorwaarden$Voorwaarde)],
      "Voorwaarde", "VoorwaardeNaam", ConnectieLSVIhabitats
    )
    assert_that(has_name(Data_voorwaarden, "Waarde"))
    if (!is.character(Data_voorwaarden$Waarde)) {
      Data_voorwaarden$Waarde <- as.character(Data_voorwaarden$Waarde)
    }

    Data_voorwaarden_NA <- Data_voorwaarden %>%
      filter(is.na(.data$Voorwaarde))

    if (nrow(Data_voorwaarden_NA) > 0) {
      if (!all(Data_voorwaarden_NA$Waarde %in% c("TRUE", "FALSE"))) {
        stop("Als je in de tabel Data_voorwaarden de kolom voorwaarde leeg laat, wordt ervan uitgegaan dat je de indicator rechtstreeks ingeschat hebt.  In dit geval mag je in de kolom Waarde enkel 'TRUE' (gunstig) of 'FALSE' (ongunstig) ingeven.  Voor minstens 1 record heb je Voorwaarde leeggelaten en bij Waarde een andere waarde dan TRUE of FALSE opgegeven") #nolint
      }
    }

    Data_voorwaarden_nietNA <- Data_voorwaarden %>%
      filter(!is.na(.data$Voorwaarde))

    if (nrow(Data_voorwaarden_nietNA) > 0) {
      assert_that(has_name(Data_voorwaarden_nietNA, "Type"))
      Data_voorwaarden_nietNA <- Data_voorwaarden_nietNA %>%
        mutate(
          Type =
            ifelse(
              is.na(.data$Type) & is.na(.data$Waarde),
              "Geheel getal",
              .data$Type
            )
        )
      if (!is.character(Data_voorwaarden_nietNA$Type)) {
        Data_voorwaarden_nietNA$Type <-
          as.character(Data_voorwaarden_nietNA$Type)
      }
      Data_voorwaarden_nietNA$Type <-
        str_to_sentence(Data_voorwaarden_nietNA$Type)
      controleerInvoerwaarde(
        "Data_voorwaarden$Type", Data_voorwaarden_nietNA$Type,
        "TypeVariabele", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
      )
      assert_that(has_name(Data_voorwaarden_nietNA, "Invoertype"))
      if (!is.character(Data_voorwaarden_nietNA$Invoertype)) {
        Data_voorwaarden_nietNA$Invoertype <-
          as.character(Data_voorwaarden_nietNA$Invoertype)
      }
      controleerInvoerwaarde(
        "Data_voorwaarden$Invoertype",
        Data_voorwaarden_nietNA$Invoertype[
          !is.na(Data_voorwaarden_nietNA$Invoertype)
        ],
        "Lijst", "Naam", ConnectieLSVIhabitats
      )
      assert_that(has_name(Data_voorwaarden_nietNA, "Eenheid"))
      if (!is.character(Data_voorwaarden_nietNA$Eenheid)) {
        Data_voorwaarden_nietNA$Eenheid <-
          as.character(Data_voorwaarden_nietNA$Eenheid)
      }
      controleerInvoerwaarde(
        "Data_voorwaarden$Eenheid", Data_voorwaarden_nietNA$Eenheid,
        "AnalyseVariabele", "Eenheid", ConnectieLSVIhabitats, Tolower = FALSE
      )

      #ingevoerde voorwaarden omzetten naar interval
      Data_voorwaarden_nietNA <- Data_voorwaarden_nietNA %>%
        mutate(
          Rijnr = row_number(.data$ID)
        )

      IntervalVoorwaarden <-
        vertaalInvoerInterval(
          Data_voorwaarden_nietNA[
            , c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")
            ],
          LIJST,
          ConnectieLSVIhabitats
        ) %>%
        rename(
          WaardeMin = .data$Min,
          WaardeMax = .data$Max
        )

      Data_voorwaarden_nietNA <- Data_voorwaarden_nietNA %>%
        left_join(
          IntervalVoorwaarden,
          by = c("Rijnr")
        ) %>%
        mutate(
          Rijnr = NULL
        )
    }

    return(list(Data_voorwaarden_NA, Data_voorwaarden_nietNA))
  }
