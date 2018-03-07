#' Invoercontrole voor dataframe Data_habitat
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).
#'
#' @param Data_habitat dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#'
invoercontroleData_habitat <- function(Data_habitat, ConnectieLSVIhabitats) {
  assert_that(inherits(Data_habitat, "data.frame"))
  assert_that(has_name(Data_habitat, "ID"))
  assert_that(has_name(Data_habitat, "Habitattype"))
  if (!all(Data_habitat$Habitattype %in%
           geefUniekeWaarden("Habitattype", "Code", ConnectieLSVIhabitats))) {
    stop("Niet alle waarden vermeld onder Data_habitat$Habitattype komen overeen met waarden vermeld in de databank.") #nolint
  }
}