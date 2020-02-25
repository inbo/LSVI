#' Invoercontrole voor dataframe Data_voorwaarden
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en
#' om de hoofdscripts overzichtelijk te houden, maken we voor elke
#' invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze
#' wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund
#' worden (maar worden wel gerund als de functie waarin ze voorkomen,
#' aangeroepen wordt).  Ingeval van Data_voorwaarden is ook de omzetting van de
#' voorwaarden naar een interval opgenomen in de functie.
#'
#' @param Data_voorwaarden dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate row_number n rename left_join
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#'
#' @export
#'
invoercontroleData_voorwaarden <- #nolint
  function(Data_voorwaarden, ConnectieLSVIhabitats, LIJST) { #nolint
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

    Dubbels <- Data_voorwaarden %>%
      group_by(.data$ID, .data$Criterium, .data$Indicator, .data$Voorwaarde) %>%
      summarise(Aantal = n()) %>%
      ungroup() %>%
      filter(.data$Aantal > 1)
    if (nrow(Dubbels) > 0) {
      Tekst <- Dubbels %>%
        group_by(.data$Voorwaarde) %>%
        summarise(
          Opname = paste(unique(.data$ID), collapse = ", ")
        ) %>%
        ungroup() %>%
        mutate(
          TekstOpname =
            paste0(
              "Voor opname(n) ", .data$Opname, " is de voorwaarde '",
              .data$Voorwaarde, "' meermaals opgegeven", collapse = NULL
            )
        ) %>%
        summarise(
          Tekst = paste(.data$TekstOpname, collapse = "; ")
        )
      stop(Tekst$Tekst)
    }

    data_voorwaarden_na <- Data_voorwaarden %>%
      filter(is.na(.data$Voorwaarde))

    data_voorwaarden_niet_na <- Data_voorwaarden %>%
      filter(!is.na(.data$Voorwaarde))

    if (nrow(data_voorwaarden_na) > 0) {
      if (!all(data_voorwaarden_na$Waarde %in% c("TRUE", "FALSE"))) {
        stop("Als je in de tabel Data_voorwaarden de kolom voorwaarde leeg laat, wordt ervan uitgegaan dat je de indicator rechtstreeks ingeschat hebt.  In dit geval mag je in de kolom Waarde enkel 'TRUE' (gunstig) of 'FALSE' (ongunstig) ingeven.  Voor minstens 1 record heb je Voorwaarde leeggelaten en bij Waarde een andere waarde dan TRUE of FALSE opgegeven") #nolint
      }
      DubbeleIndicatoren <- data_voorwaarden_na %>%
        inner_join(
          data_voorwaarden_niet_na,
          by = c("ID", "Criterium", "Indicator")
        )
      if (nrow(DubbeleIndicatoren) > 0) {
        Tekst <- DubbeleIndicatoren %>%
          group_by(.data$Indicator) %>%
          summarise(
            Opname = paste(unique(.data$ID), collapse = ", ")
          ) %>%
          ungroup() %>%
          mutate(
            TekstOpname =
              paste0(
                "Voor opname(n) ", .data$Opname, " is de indicator '",
                .data$Indicator,
                "' tweemaal opgegeven: rechtstreeks ingeschat als indicator (TRUE/FALSE) en door het opgeven van concrete waarden voor een van de voorwaarden", #nolint
                collapse = NULL
              )
          ) %>%
          summarise(
            Tekst = paste(.data$TekstOpname, collapse = "; ")
          )
        stop(Tekst$Tekst)
      }
    }

    if (nrow(data_voorwaarden_niet_na) > 0) {
      assert_that(has_name(data_voorwaarden_niet_na, "Type"))
      data_voorwaarden_niet_na <- data_voorwaarden_niet_na %>%
        mutate(
          Type =
            ifelse(
              is.na(.data$Type) & is.na(.data$Waarde),
              "Geheel getal",
              .data$Type
            )
        )
      if (!is.character(data_voorwaarden_niet_na$Type)) {
        data_voorwaarden_niet_na$Type <-
          as.character(data_voorwaarden_niet_na$Type)
      }
      data_voorwaarden_niet_na$Type <-
        str_to_sentence(data_voorwaarden_niet_na$Type)
      controleerInvoerwaarde(
        "Data_voorwaarden$Type", data_voorwaarden_niet_na$Type,
        "TypeVariabele", "Naam", ConnectieLSVIhabitats, Tolower = FALSE
      )
      assert_that(has_name(data_voorwaarden_niet_na, "Invoertype"))
      if (!is.character(data_voorwaarden_niet_na$Invoertype)) {
        data_voorwaarden_niet_na$Invoertype <-
          as.character(data_voorwaarden_niet_na$Invoertype)
      }
      controleerInvoerwaarde(
        "Data_voorwaarden$Invoertype",
        data_voorwaarden_niet_na$Invoertype[
          !is.na(data_voorwaarden_niet_na$Invoertype)
        ],
        "Lijst", "Naam", ConnectieLSVIhabitats
      )
      assert_that(has_name(data_voorwaarden_niet_na, "Eenheid"))
      if (!is.character(data_voorwaarden_niet_na$Eenheid)) {
        data_voorwaarden_niet_na$Eenheid <-
          as.character(data_voorwaarden_niet_na$Eenheid)
      }
      controleerInvoerwaarde(
        "Data_voorwaarden$Eenheid", data_voorwaarden_niet_na$Eenheid,
        "AnalyseVariabele", "Eenheid", ConnectieLSVIhabitats, Tolower = FALSE
      )

      #ingevoerde voorwaarden omzetten naar interval
      data_voorwaarden_niet_na <- data_voorwaarden_niet_na %>%
        mutate(
          Rijnr = row_number(.data$ID)
        )

      IntervalVoorwaarden <-
        vertaalInvoerInterval(
          data_voorwaarden_niet_na[
            , c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")
            ],
          LIJST,
          ConnectieLSVIhabitats
        ) %>%
        rename(
          WaardeMin = .data$Min,
          WaardeMax = .data$Max
        )

      data_voorwaarden_niet_na <- data_voorwaarden_niet_na %>%
        left_join(
          IntervalVoorwaarden,
          by = c("Rijnr")
        ) %>%
        mutate(
          Rijnr = NULL
        )
    }

    return(list(data_voorwaarden_na, data_voorwaarden_niet_na))
  }
