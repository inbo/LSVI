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
    if (!all(Data_voorwaarden$Criterium %in%
             geefUniekeWaarden("Criterium", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Criterium komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Indicator"))
    if (!is.character(Data_voorwaarden$Indicator)) {
      Data_voorwaarden$Indicator <- as.character(Data_voorwaarden$Indicator)
    }
    if (!all(Data_voorwaarden$Indicator %in%
             geefUniekeWaarden("Indicator", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Indicator komen overeen met waarden vermeld in de databank.") #nolint
    }
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
    Geldigewaarden <-
      tolower(
        geefUniekeWaarden("Voorwaarde", "VoorwaardeNaam", ConnectieLSVIhabitats)
      )
    if (
      !all(
        Data_voorwaarden$Voorwaarde %in%
          Geldigewaarden
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Voorwaarde komen overeen met waarden vermeld in de databank.") #nolint
    }
    #misschien best ook testen dat die indicator-criterium-combinatie in de databank voorkomt?  En of deze voor dat habitattype voorkomt, maar dat best verderop doen
    #Voorwaarde ook verplichten?  Anders wel testen of het ok is als het aanwezig is.
    assert_that(has_name(Data_voorwaarden, "Waarde"))
    assert_that(has_name(Data_voorwaarden, "Type"))
    if (!is.character(Data_voorwaarden$Type)) {
      Data_voorwaarden$Type <- as.character(Data_voorwaarden$Type)
    }
    if (
      !all(
        Data_voorwaarden$Type %in%
        geefUniekeWaarden("TypeVariabele", "Naam", ConnectieLSVIhabitats)
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Type komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Invoertype"))
    if (!is.character(Data_voorwaarden$Invoertype)) {
      Data_voorwaarden$Invoertype <- as.character(Data_voorwaarden$Invoertype)
    }
    if (!all(is.na(Data_voorwaarden$Invoertype) |
             tolower(Data_voorwaarden$Invoertype) %in%
             tolower(
               geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats)))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Eenheid"))
    if (!is.character(Data_voorwaarden$Eenheid)) {
      Data_voorwaarden$Eenheid <- as.character(Data_voorwaarden$Eenheid)
    }
    if (
      !all(
        Data_voorwaarden$Eenheid %in%
        geefUniekeWaarden(
          "AnalyseVariabele",
          "Eenheid",
          ConnectieLSVIhabitats
        )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Eenheid komen overeen met waarden vermeld in de databank.") #nolint
    }

    #ingevoerde voorwaarden omzetten naar interval
    Data_voorwaarden <- Data_voorwaarden %>%
      mutate(
        Rijnr = row_number(.data$ID)
      )

    IntervalVoorwaarden <-
      vertaalInvoerInterval(
        Data_voorwaarden[
          , c("Rijnr", "Type", "Waarde", "Eenheid", "Invoertype")
          ],
        LIJST,
        ConnectieLSVIhabitats
      ) %>%
      rename(
        WaardeMin = .data$Min,
        WaardeMax = .data$Max
      )

    Data_voorwaarden <- Data_voorwaarden %>%
      left_join(
        IntervalVoorwaarden,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL
      )

    return(Data_voorwaarden)
  }