#' @title zet ingevoerde gegevens om naar een interval
#'
#' @description Deze functie zet ingevoerde gegevens van meerdere types om naar een interval bestaande uit minimumwaarde en maximumwaarde.  De functie gebruikt Type, Eenheid en Invoertype om te bepalen welke omzetting eventueel nodig is.  Percentages worden bv. omgezet naar een decimaal getal (waarbij minimum en maximum dezelfde waarde zullen krijgen), en categorische variabelen met Invoertype Tansley worden omgezet naar de onder- en bovengrens die in de databank gegeven worden voor de betreffende categorie (op basis van parameter LIJST).
#' 
#' @param Dataset dataframe met velden Rijnr, Type, Waarde, Eenheid en Invoertype
#' @inheritParams berekenLSVIbasis
#' 
#' @return Dataframe met velden Min
#' 
#' @examples 
#' 
#' 
#' @export
#' 
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% left_join mutate select filter bind_rows as.tbl
#' 

vertaalInvoerInterval <-
  function(Dataset, LIJST, ConnectieLSVIhabitats){

    colnames(Dataset) <- c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")

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
    assert_that(has_name(Dataset, "Waarde"))
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
      mutate(
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
      )

    Resultaat <- Dataset %>%
      filter(tolower(.data$Type) == "categorie")

    if (nrow(Resultaat) > 0) {
      Resultaat <- Resultaat %>%
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
    }

    Resultaat <- Resultaat %>%
      bind_rows(
        Dataset %>%
          filter(tolower(.data$Type) == "percentage") %>%
          mutate(
            Min = as.numeric(gsub(",", ".", .data$Waarde)) / 100,
            Max = Min
          )
      ) %>%
      bind_rows(
        Dataset %>%
          filter(!tolower(.data$Type) %in% c("categorie", "percentage")) %>%
          mutate(
            Min = as.numeric(gsub(",", ".", .data$Waarde)),
            Max = Min
          )
      ) %>%
      select(
        .data$Rijnr,
        .data$Min,
        .data$Max
      )

    return(Resultaat)

  }
