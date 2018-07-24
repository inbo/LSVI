#' @title zet een interval om naar een waarde in de gevraagde eenheid
#'
#' @description Deze functie zet een interval bestaande uit minimumwaarde en maximumwaarde om naar een uitvoerwaarde in de opgegeven eenheid.  De functie gebruikt Type, Eenheid en Invoertype om te bepalen welke omzetting eventueel nodig is.  Als minimum en maximum niet dezelfde waarde hebben, geeft ze beide waarden weer, gescheiden door een '-'.
#' 
#' @param Dataset dataframe met velden Rijnr, Type, Min, Max, Eenheid en Invoertype
#' @inheritParams berekenLSVIbasis
#' 
#' @return Dataframe met velden Min
#' 
#' @export
#' 
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate select filter arrange bind_rows as.tbl
#' @importFrom rlang .data
#' 

vertaalIntervalUitvoer <-
  function(Dataset, LIJST, ConnectieLSVIhabitats){

    colnames(Dataset) <-
      c("Rijnr", "Type", "Min", "Max", "Eenheid", "Invoertype")

    assert_that(inherits(Dataset, "data.frame"))
    assert_that(has_name(Dataset, "Rijnr"))
    assert_that(has_name(Dataset, "Type"))
    GeldigeTypes <-
      geefUniekeWaarden("TypeVariabele", "Naam", ConnectieLSVIhabitats)
    GeldigeTypes <- GeldigeTypes[!GeldigeTypes == "alle"]
    if (!all(tolower(Dataset$Type) %in% tolower(GeldigeTypes))) {
      stop(
        "Het opgegeven type is geen geldige waarde. Geef een van de volgende waarden op: ",  #nolint
        paste(
          GeldigeTypes,
          collapse = ", "
        )
      )
    }
    assert_that(has_name(Dataset, "Min"))
    assert_that(has_name(Dataset, "Max"))
    assert_that(has_name(Dataset, "Eenheid"))
    assert_that(has_name(Dataset, "Invoertype"))
    GeldigeTypes <-
      geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats)
    GeldigeTypes <- c(GeldigeTypes[!GeldigeTypes == "alle"], NA)
    if (!all(tolower(Dataset$Invoertype) %in% tolower(GeldigeTypes))) {
      stop(
        "Het opgegeven invoertype is geen geldige waarde. Geef een van de volgende waarden op: ",  #nolint
        paste(
          GeldigeTypes,
          collapse = ", "
        )
      )
    }

    LIJST <- LIJST %>%
      as.tbl() %>%
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
        .data$Naam,
        .data$Waarde,
        .data$Ondergrens,
        .data$Bovengrens
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
      Waarde <-
        cut(
          Waarde,
          breaks = c(Lijstdetail$Ondergrens, max(Lijstdetail$Bovengrens)),
          labels = Lijstdetail$Waarde
        )
      Waarde <- as.character(Waarde)
      return(Waarde)
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
            omzetfunctie(.data$Gem, .data$Invoertype)
        ) %>%
        ungroup() %>%
        mutate(
          #Waarde = as.character(.data$Waarde),
          Gem = NULL
        )

      if (max(is.na(Resultaat$Waarde) & !is.na(Resultaat$Max))) {
        warning("Er ging iets mis bij de omzetting van het berekend resultaat naar een categorische waarde (functie vertaalIntervalUitvoer).")  #nolint
      }
    }

    Resultaat <- Resultaat %>%
      bind_rows(
        Dataset %>%
          filter(!tolower(.data$Type) == "categorie") %>%
          mutate(
            Min =
              ifelse(
                .data$Type == "percentage",
                .data$Min * 100,
                .data$Min
              ),
            Max =
              ifelse(
                .data$Type == "percentage",
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
              )
          )
      ) %>%
      select(
        .data$Rijnr,
        .data$Waarde
      )

    return(Resultaat)

  }
