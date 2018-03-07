#' Invoercontrole voor dataframe Data_voorwaarden
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).
#'
#' @param Data_voorwaarden dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#'
invoercontroleData_voorwaarden <- 
  function(Data_voorwaarden, ConnectieLSVIhabitats) {
    assert_that(inherits(Data_voorwaarden, "data.frame"))
    assert_that(has_name(Data_voorwaarden, "ID"))
    assert_that(has_name(Data_voorwaarden, "Criterium"))
    if (!all(Data_voorwaarden$Criterium %in%
             geefUniekeWaarden("Criterium", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Criterium komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Indicator"))
    if (!all(Data_voorwaarden$Indicator %in%
             geefUniekeWaarden("Indicator", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Indicator komen overeen met waarden vermeld in de databank.") #nolint
    }
    #misschien best ook testen dat die indicator-criterium-combinatie in de databank voorkomt?  En of deze voor dat habitattype voorkomt, maar dat best verderop doen
    #Voorwaarde ook verplichten?  Anders wel testen of het ok is als het aanwezig is.
    assert_that(has_name(Data_voorwaarden, "Waarde"))
    assert_that(has_name(Data_voorwaarden, "Type"))
    if (
      !all(
        Data_voorwaarden$Type %in%
        geefUniekeWaarden("TypeVariabele", "Naam", ConnectieLSVIhabitats)
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Type komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Invoertype"))
    if (!all(is.na(Data_voorwaarden$Invoertype) |
             Data_voorwaarden$Invoertype %in%
             geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_voorwaarden$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_voorwaarden, "Eenheid"))
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
    
  }