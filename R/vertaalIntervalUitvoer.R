#' @title zet een interval om naar een waarde in de gevraagde eenheid
#'
#' @description Deze functie zet een interval bestaande uit minimumwaarde en
#' maximumwaarde om naar een uitvoerwaarde in de opgegeven eenheid.  De functie
#' gebruikt Type, Eenheid en Invoertype om te bepalen welke omzetting eventueel
#' nodig is.  Als minimum en maximum niet dezelfde waarde hebben, geeft ze
#' beide waarden weer, gescheiden door een '-'.
#'
#' @param Dataset dataframe met velden Rijnr, Type, Min, Max, Eenheid en
#' Invoertype
#' @param LIJST Dataframe met lijst die weergeeft hoe de vertaling moet
#' gebeuren van numerieke waarden naar categorische variabelen.  Verschillend
#' van andere functies die dezelfde lijst gebruiken, mogen hier geen
#' overlappende categorieen voorkomen binnen eenzelfde schaal.  Om zulke lijst
#' te bekomen, moeten uit de lijst gegenereerd door de functie
#' vertaalInvoerInterval() de records met Basisschaal 1 gefilterd worden.
#' @inheritParams berekenLSVIbasis
#'
#' @return Dataframe met velden Min
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate select filter arrange bind_rows
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#'

vertaalIntervalUitvoer <-
  function(Dataset, LIJST, ConnectieLSVIhabitats) {

    colnames(Dataset) <-
      c("Rijnr", "Type", "Min", "Max", "Eenheid", "Invoertype")

    assert_that(inherits(Dataset, "data.frame"))
    assert_that(has_name(Dataset, "Rijnr"))
    assert_that(has_name(Dataset, "Type"))
    Dataset$Type <- ifelse(Dataset$Type == "alle", "fouteInvoer", Dataset$Type)
    controleerInvoerwaarde(
      "Type", Dataset$Type, "TypeVariabele", "Naam", ConnectieLSVIhabitats
    )
    assert_that(has_name(Dataset, "Min"))
    assert_that(has_name(Dataset, "Max"))
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
      filter(!is.na(.data$Ondergrens)) %>%
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
      ) %>%
      arrange(
        .data$Naam,
        .data$Ondergrens
      )

    Resultaat <- Dataset %>%
      filter(tolower(.data$Type) == "categorie")

    omzetfunctie <- function(Waarde, Invoertype) {
      Lijstdetail <- LIJST %>%
        filter(.data$Naam == Invoertype)
      WaardeSchaal <-
        cut(
          Waarde,
          breaks = c(Lijstdetail$Ondergrens, max(Lijstdetail$Bovengrens)),
          labels = Lijstdetail$Waarde
        )
      WaardeSchaal <- as.character(WaardeSchaal)
      WaardeSchaal <-
        ifelse(
          is.na(WaardeSchaal) & Waarde == 0,
          "Afwezig",
          WaardeSchaal
        )
      return(WaardeSchaal)
    }

    if (nrow(Resultaat) > 0) {
      Resultaat <- Resultaat %>%
        mutate(
          Invoertype = tolower(.data$Invoertype)
        ) %>%
        mutate(
          Gem = (.data$Min + .data$Max) / 2
        ) %>%
        group_by(.data$Invoertype) %>%
        mutate(
          Waarde =
            omzetfunctie(.data$Gem, unique(.data$Invoertype))
        ) %>%
        ungroup() %>%
        mutate(
          Waarde =
            ifelse(
              is.na(.data$Waarde) & .data$Gem == 0,
              "0",
              .data$Waarde
            ),
          Gem = NULL
        )

      if (max(is.na(Resultaat$Waarde) & !is.na(Resultaat$Max))) {
        warning("Er ging iets mis bij de omzetting van het berekend resultaat naar een categorische waarde (functie vertaalIntervalUitvoer).")  #nolint
      }
    }

    Resultaat <- Resultaat %>%
      bind_rows(
        Dataset %>%
          filter(tolower(.data$Type) != "categorie") %>%
          mutate(
            Min =
              ifelse(
                tolower(.data$Type) == "percentage",
                .data$Min * 100,
                .data$Min
              ),
            Max =
              ifelse(
                tolower(.data$Type) == "percentage",
                .data$Max * 100,
                .data$Max
              ),
            Waarde =
              ifelse(
                .data$Min == .data$Max,
                as.character(.data$Min),
                paste(
                  round(.data$Min, 1),
                  round(.data$Max, 1),
                  sep = " - ")
              ),
            Waarde = as.character(Waarde)
          )
      ) %>%
      select(
        "Rijnr",
        "Waarde"
      )

    return(Resultaat)

  }
