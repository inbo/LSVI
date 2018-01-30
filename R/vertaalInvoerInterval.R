#' @title zet ingevoerde gegevens om naar een interval
#'
#' @description Deze functie zet ingevoerde gegevens van meerdere types om naar een interval bestaande uit minimumwaarde en maximumwaarde.  De functie gebruikt Type, Eenheid en Invoertype om te bepalen welke omzetting eventueel nodig is.  Percentages worden bv. omgezet naar een decimaal getal (waarbij minimum en maximum dezelfde waarde zullen krijgen), en categorische variabelen met Invoertype Tansley worden omgezet naar de onder- en bovengrens die in de databank gegeven worden voor de betreffende categorie.
#' 
#' @param Dataset dataframe met velden Type, Waarde, Eenheid en Invoertype
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
  function(Dataset, LIJST){

    colnames(Dataset) <- c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")

    #nog herschrijven
    # assert_that(inherits(Dataset, "data.frame"))
    # assert_that(has_name(Dataset, "Type"))
    # GeldigeTypes <- 
    #   geefUniekeWaarden(ConnectieLSVIhabitats, "TypeVariabele", "Naam")
    # GeldigeTypes <- GeldigeTypes[!GeldigeTypes == "alle"]
    # if (!tolower(Dataset$Type) %in% tolower(GeldigeTypes)) {
    #   stop(
    #     "Het opgegeven type is geen geldige waarde. Geef een van de volgende waarden op: ",
    #     paste(
    #       GeldigeTypes,
    #       collapse = ", "
    #     )
    #   )
    # }
    # assert_that(has_name(Dataset, "Waarde"))
    # assert_that(has_name(Dataset, "Eenheid"))
    # assert_that(has_name(Dataset, "Invoertype"))
    # GeldigeTypes <- 
    #   geefUniekeWaarden(ConnectieLSVIhabitats, "Lijst", "Naam")
    # GeldigeTypes <- GeldigeTypes[!GeldigeTypes == "alle"]
    # if (!tolower(Dataset$Invoertype) %in% tolower(GeldigeTypes)) {
    #   stop(
    #     "Het opgegeven invoertype is geen geldige waarde. Geef een van de volgende waarden op: ",
    #     paste(
    #       GeldigeTypes,
    #       collapse = ", "
    #     )
    #   )
    # }

    LIJST <- LIJST %>%
      as.tbl() %>%
      filter(Naam == "Tansley IHD") %>%
      mutate(
        Ondergrens = as.numeric(.data$Ondergrens),
        Ondergrens =
          ifelse(
            is.na(.data$Ondergrens),
            .data$Gemiddelde,
            .data$Ondergrens
          ),
        #Ondergrens = ifelse(is.na(.data$Ondergrens), .data$Volgnummer, .data$Ondergrens),
        Bovengrens = as.numeric(.data$Bovengrens),
        Bovengrens =
          ifelse(
            is.na(.data$Bovengrens),
            .data$Gemiddelde,
            .data$Bovengrens
          )
        #Bovengrens = ifelse(is.na(.data$Bovengrens), .data$Volgnummer, .data$Bovengrens)
      ) %>%
      select(
        .data$Naam,
        .data$Waarde,
        .data$Ondergrens,
        .data$Bovengrens
      )

    Resultaat <- Dataset %>%
      filter(tolower(.data$Type) == "categorie") %>%
      left_join(
        LIJST,
        by = c("Invoertype" = "Naam", "Waarde" = "Waarde")
      ) %>%
      mutate(
        Min = .data$Ondergrens,
        Max = .data$Bovengrens,
        Ondergrens = NULL,
        Bovengrens = NULL
      ) %>%
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
