#' Invoercontrole voor dataframe Data_soortenKenmerken
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).
#'
#' @param Data_soortenKenmerken dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#'
invoercontroleData_soortenKenmerken <- 
  function(Data_soortenKenmerken, ConnectieLSVIhabitats) {
    assert_that(inherits(Data_soortenKenmerken, "data.frame"))
    assert_that(has_name(Data_soortenKenmerken, "ID"))
    assert_that(has_name(Data_soortenKenmerken, "Kenmerk"))
    assert_that(has_name(Data_soortenKenmerken, "TypeKenmerk"))
    #hier moet nog controle op gebeuren!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    assert_that(has_name(Data_soortenKenmerken, "Waarde"))
    assert_that(has_name(Data_soortenKenmerken, "Type"))
    if (
      !all(Data_soortenKenmerken$Type %in%
           geefUniekeWaarden(
             "TypeVariabele",
             "Naam",
             ConnectieLSVIhabitats
           )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Type komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_soortenKenmerken, "Invoertype"))
    if (!all(is.na(Data_soortenKenmerken$Invoertype) |
             Data_soortenKenmerken$Invoertype %in%
             geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_soortenKenmerken, "Eenheid"))
    if (
      !all(
        Data_soortenKenmerken$Eenheid %in%
        geefUniekeWaarden(
          "AnalyseVariabele",
          "Eenheid",
          ConnectieLSVIhabitats
        )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Eenheid komen overeen met waarden vermeld in de databank.") #nolint
    }
  }