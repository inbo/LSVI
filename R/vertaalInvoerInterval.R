#' @title zet ingevoerde gegevens om naar een interval
#'
#' @description Deze functie zet ingevoerde gegevens van meerdere types om naar
#' een interval bestaande uit minimumwaarde en maximumwaarde.  De functie
#' gebruikt Type, Eenheid en Invoertype om te bepalen welke omzetting eventueel
#' nodig is.  Percentages worden bv. omgezet naar een decimaal getal (waarbij
#' minimum en maximum dezelfde waarde zullen krijgen), en categorische
#' variabelen met Invoertype Tansley worden omgezet naar de onder- en
#' bovengrens die in de databank gegeven worden voor de betreffende categorie
#' (op basis van parameter LIJST).  Om een onderscheid te maken tussen
#' numerieke waarden en aan-/afwezigheid, wordt voor deze laatste enkel de
#' minimumwaarde ingevoerd (dus maximum = NA).
#'
#' @param Dataset dataframe met velden Rijnr, Type, Waarde, Eenheid en
#' Invoertype
#' @inheritParams berekenLSVIbasis
#'
#' @return Dataframe met velden Rijnr, Min en Max
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% left_join mutate select filter bind_rows
#' @importFrom stringr str_split_fixed
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#'

vertaalInvoerInterval <-
  function(Dataset, LIJST, ConnectieLSVIhabitats) {

    colnames(Dataset) <- c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")

    assert_that(inherits(Dataset, "data.frame"))
    assert_that(has_name(Dataset, "Rijnr"))
    assert_that(has_name(Dataset, "Type"))
    Dataset$Type <- ifelse(Dataset$Type == "alle", "fouteInvoer", Dataset$Type)
    controleerInvoerwaarde(
      "Type", Dataset$Type, "TypeVariabele", "Naam", ConnectieLSVIhabitats
    )
    assert_that(has_name(Dataset, "Waarde"))
    assert_that(has_name(Dataset, "Eenheid"))
    assert_that(has_name(Dataset, "Invoertype"))
    Dataset$Invoertype <-
      ifelse(Dataset$Invoertype == "alle", "fouteInvoer", Dataset$Invoertype)
    controleerInvoerwaarde(
      "Invoertype", Dataset$Invoertype[!is.na(Dataset$Invoertype)],
      "Lijst", "Naam", ConnectieLSVIhabitats
    )

    LIJST <- LIJST %>%
      as_tibble() %>%
      mutate(
        Naam = tolower(.data$Naam),
        Waarde = tolower(.data$Waarde),
        Ondergrens = as.numeric(.data$Ondergrens),
        Ondergrens =
          ifelse(
            is.na(.data$Ondergrens),
            .data$Gemiddelde,
            .data$Ondergrens
          ),
        Bovengrens = as.numeric(.data$Bovengrens),
        Bovengrens =
          ifelse(
            is.na(.data$Bovengrens),
            .data$Gemiddelde,
            .data$Bovengrens
          )
      ) %>%
      select(
        "Naam",
        "Waarde",
        "Ondergrens",
        "Bovengrens"
      )

    Resultaat <- Dataset %>%
      filter(tolower(.data$Type) == "categorie")

    if (nrow(Resultaat) > 0) {
      Resultaat <- Resultaat %>%
        mutate(
          Invoertype = tolower(.data$Invoertype),
          Waarde = tolower(.data$Waarde)
        ) %>%
        left_join(
          LIJST,
          by = c("Invoertype" = "Naam", "Waarde" = "Waarde")
        ) %>%
        mutate(
          Min = .data$Ondergrens,
          Max = .data$Bovengrens,
          Ondergrens = NULL,
          Bovengrens = NULL
        )

      if (max(is.na(Resultaat$Min) & !is.na(Resultaat$Waarde))) {
        warning("Niet voor elke opgegeven categorische variabele is er een numerieke waarde opgenomen in de databank (zie functie geefVertaallijst(ConnectiePool)), waardoor niet voor elke waarde een berekening gemaakt kan worden. Controleer de spelling van de categorische variabele, of neem contact op met de beheerder van het package om nieuwe numerieke waarden aan te leveren.")  #nolint
      }
    }

    waardeNaarGetal <- function(x) {
      y <-
        tryCatch(
          as.numeric(gsub(",", ".", x)),
          warning = function(w) {
            if (grepl("NAs introduced by coercion", w)) {
              stop("Niet alle opgegeven getallen en percentages zijn numerieke waarden") #nolint
            } else {
              as.numeric(gsub(",", ".", x))
            }
          }
        )
      return(y)
    }

    DatasetNumeriek <- Dataset %>%
      filter(
        tolower(.data$Type) %in%
          c("percentage", "geheel getal", "decimaal getal")
      ) %>%
      mutate(
        Min =
          ifelse(
            grepl("-", .data$Waarde),
            str_split_fixed(.data$Waarde, "-", 2)[, 1],
            .data$Waarde
          ),
        Max =
          ifelse(
            grepl("-", .data$Waarde),
            str_split_fixed(.data$Waarde, "-", 2)[, 2],
            .data$Waarde
          ),
        Min =
          ifelse(
            tolower(.data$Type) == "percentage",
            waardeNaarGetal(.data$Min) / 100,
            waardeNaarGetal(.data$Min)
          ),
        Max =
          ifelse(
            tolower(.data$Type) == "percentage",
            waardeNaarGetal(.data$Max) / 100,
            waardeNaarGetal(.data$Max)
          ),
        Min =
          ifelse(
            is.na(.data$Min) & !is.na(.data$Max),
            stop("Niet alle opgegeven getallen en percentages zijn positieve waarden"), #nolint
            .data$Min
          )
      )

    Resultaat <- Resultaat %>%
      bind_rows(
        DatasetNumeriek
      ) %>%
      bind_rows(
        Dataset %>%
          filter(tolower(.data$Type) == "ja/nee") %>%
          mutate(
            Min =
              tryCatch(
                as.numeric(.data$Waarde),
                warning = function(w) {
                  if (grepl("NAs introduced by coercion", w)) {
                    stop("Minstens een van de opgegeven Ja/nee-waarden bevat tekst") #nolint
                  } else {
                    as.numeric(.data$Waarde)
                  }
                }
              ),
            Max = NA
          )
      )

    ResultaatJaNee <- Resultaat %>%
      filter(tolower(.data$Type) == "ja/nee")

    if (nrow(ResultaatJaNee) > 0) {
      if (!all(ResultaatJaNee$Min %in% c(0, 1))) {
        stop("Niet alle opgegeven Ja/nee-waarden bevatten waarden die door R vertaald kunnen worden naar TRUE of FALSE.") #nolint
      }
    }

    Resultaat <- Resultaat %>%
      select(
        "Rijnr",
        "Min",
        "Max"
      )

    return(Resultaat)

  }
